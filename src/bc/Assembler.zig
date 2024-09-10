const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const Register = Bytecode.Register;

const Assembler = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: Bytecode.List,
register_count: u32,
register_map: std.AutoHashMapUnmanaged(Ir.Index, Bytecode.Register),
free_registers: std.ArrayListUnmanaged(Bytecode.Register),
// some instructions, like phis, keep track of the mapping
// from Ir index to bytecode index, so they can be updated later
inst_map: std.AutoHashMapUnmanaged(Ir.Index, u32),
scratch: std.ArrayListUnmanaged(u32),

const Slot = struct {
    inst: Ir.Index,
    live: bool,
};

pub fn assemble(gpa: Allocator, pool: *InternPool, ir: *const Ir) !Bytecode {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var assembler: Assembler = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .pool = pool,
        .ir = ir,
        .code = .{},
        .register_count = 0,
        .free_registers = .{},
        .register_map = .{},
        .inst_map = .{},
        .scratch = .{},
    };
    try assembler.generateBlock(ir.block);
    try assembler.add(.exit, undefined);

    return .{
        .ir = ir,
        .code = assembler.code.toOwnedSlice(),
    };
}

fn generateBlock(self: *Assembler, index: Ir.ExtraIndex) error{OutOfMemory}!void {
    const ir = self.ir;
    const block = ir.extraData(Ir.Inst.ExtraSlice, index);
    const insts = ir.extraSlice(block);
    for (insts) |inst| {
        try self.generateInst(@enumFromInt(inst));
    }
}

fn generateInst(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const dead_bits = ir.liveness.deadBits(inst);

    const index = @intFromEnum(inst);
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        .itof => try self.itof(inst),
        .ftoi => try self.ftoi(inst),
        .neg,
        .binv,
        .lnot,
        => try self.unaryOp(inst),
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        .bor,
        .band,
        .bxor,
        .sll,
        .sra,
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        => try self.binaryOp(inst),
        .lor => try self.lor(inst),
        .land => try self.land(inst),
        .ret => unreachable, // TODO: implement
        .if_else => try self.ifElse(inst),
        .loop => try self.loop(inst),
        .phi_if_else, .phi_entry_if, .phi_entry_else => {},
    }

    switch (ir.insts.items(.tag)[index]) {
        .if_else, .loop => {},
        else => if (dead_bits & 0x8 != 0) self.unassign(inst),
    }
}

inline fn add(self: *Assembler, tag: Bytecode.Inst.Tag, payload: Bytecode.Inst.Payload) !void {
    try self.code.append(self.gpa, .{ .tag = tag, .payload = payload });
}

inline fn reserve(self: *Assembler, tag: Bytecode.Inst.Tag) !Bytecode.Register {
    const index: u32 = @intCast(self.code.len);
    try self.code.append(self.gpa, .{ .tag = tag, .payload = undefined });
    return @enumFromInt(index);
}

inline fn update(self: *Assembler, inst: Bytecode.Register, payload: Bytecode.Inst.Payload) void {
    const i = @intFromEnum(inst);
    self.code.items(.payload)[i] = payload;
}

fn allocate(self: *Assembler) !Register {
    // try to allocate an existing register (doesn't matter which)
    // before increasing our stack frame size
    if (self.free_registers.items.len > 0) {
        const register = self.free_registers.swapRemove(0);
        return register;
    }

    // no free register found, so "spill" by increasing our stack frame size
    const register: Register = @enumFromInt(self.register_count);
    self.register_count += 1;
    try self.free_registers.ensureTotalCapacity(self.arena, self.register_count);
    return register;
}

fn deallocate(self: *Assembler, register: Register) void {
    self.free_registers.appendAssumeCapacity(register);
}

fn assign(self: *Assembler, inst: Ir.Index) !Bytecode.Register {
    if (self.free_registers.items.len > 0) {
        const register = self.free_registers.swapRemove(0);
        self.register_map.putAssumeCapacity(inst, register);
        return register;
    }

    const index: u32 = self.register_count;
    self.register_count += 1;
    try self.free_registers.ensureUnusedCapacity(self.arena, 1);
    try self.register_map.ensureTotalCapacity(self.arena, @intCast(self.free_registers.capacity));

    const register: Bytecode.Register = @enumFromInt(index);
    self.register_map.putAssumeCapacity(inst, register);
    return register;
}

fn unassign(self: *Assembler, inst: Ir.Index) void {
    if (!self.register_map.contains(inst)) std.debug.print("no: {}\n", .{@intFromEnum(inst)});
    const index = self.register_map.get(inst).?;
    std.debug.assert(self.register_map.remove(inst));
    self.free_registers.appendAssumeCapacity(index);
}

fn unassignRegister(self: *Assembler, index: Bytecode.Register) void {
    self.free_registers.appendAssumeCapacity(index);
}

fn getSlot(self: *Assembler, inst: Ir.Index) Bytecode.Register {
    if (!self.register_map.contains(inst)) std.debug.print("no: {}\n", .{@intFromEnum(inst)});
    return self.register_map.get(inst).?;
}

fn constant(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const tv = self.pool.get(ip).tv;

    const wide = switch (tv.ty) {
        .int => tv.val.int > std.math.maxInt(u32),
        .float => true,
        else => false,
    };
    if (wide) {
        var imm: [8]u8 = undefined;
        switch (tv.ty) {
            .int => @memcpy(&imm, asBytes(&tv.val.int)),
            .float => @memcpy(&imm, asBytes(&tv.val.float)),
            else => unreachable,
        }
        const dst = try self.allocate();
        try self.register_map.put(self.arena, inst, dst);
        try self.add(.ldw, .{
            .dst = dst,
            .ops = .{ .wimm = imm },
        });
    } else {
        var imm: [4]u8 = undefined;
        const val: u32 = switch (tv.ty) {
            .nonetype => undefined,
            .bool => @intFromBool(tv.val.bool),
            .int => @intCast(tv.val.int),
            else => unreachable,
        };
        @memcpy(&imm, asBytes(&val));
        const dst = try self.allocate();
        try self.register_map.put(self.arena, inst, dst);
        try self.add(.ld, .{
            .dst = dst,
            .ops = .{ .imm = imm },
        });
    }
}

fn itof(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(.itof, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

fn ftoi(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(.ftoi, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

fn binaryOp(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const ty = self.pool.get(self.ir.typeOf(binary.l)).ty;
    const tag: Bytecode.Inst.Tag = switch (ty) {
        .int => switch (self.ir.instTag(inst)) {
            .add => .iadd,
            .sub => .isub,
            .mul => .imul,
            .div => .idiv,
            .mod => .imod,
            .pow => .ipow,
            .band => .band,
            .bor => .bor,
            .bxor => .bxor,
            .sll => .sll,
            .sra => .sra,
            .eq => .ieq,
            .ne => .ine,
            .lt => .ilt,
            .gt => .igt,
            .le => .ile,
            .ge => .ige,
            else => unreachable,
        },
        .float => switch (self.ir.instTag(inst)) {
            .add => .fadd,
            .sub => .fsub,
            .mul => .fmul,
            .div => .fdiv,
            .mod => .fmod,
            .pow => .fpow,
            .eq, .ne => unreachable,
            .lt => .flt,
            .gt => .fgt,
            .le => .fle,
            .ge => .fge,
            else => unreachable,
        },
        else => unreachable,
    };

    const l = self.register_map.get(binary.l).?;
    const r = self.register_map.get(binary.r).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(l);
    if (dead_bits & 0x2 != 0) self.deallocate(r);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(tag, .{
        .dst = dst,
        .ops = .{
            .binary = .{
                .op1 = l,
                .op2 = r,
            },
        },
    });
}

fn unaryOp(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const ty = self.pool.get(self.ir.typeOf(unary)).ty;
    const tag: Bytecode.Inst.Tag = switch (self.ir.instTag(inst)) {
        .neg => switch (ty) {
            .int => .ineg,
            .float => .fneg,
            else => unreachable,
        },
        .binv => .binv,
        .lnot => .lnot,
        else => unreachable,
    };

    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(tag, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

// TODO: this is just plain wrong, this logic needs to be done in Ir since
// otherwise the operands are still eagerly evaluated
fn lor(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);

    var one: [4]u8 = undefined;
    @memcpy(asBytes(&one), asBytes(&@as(u32, 1)));
    const dst = try self.allocate();
    try self.add(.ld, .{
        .dst = dst,
        .ops = .{ .imm = one },
    });

    // if the first operand is true, skip checking the second one
    const op1 = self.getSlot(binary.l);
    if (dead_bits & 0x1 != 0) self.unassign(binary.l);
    const branch = try self.reserve(.branch); // will consume op1

    const op2 = self.getSlot(binary.r);
    if (dead_bits & 0x2 != 0) self.unassign(binary.r);
    try self.add(.mov, .{ .dst = dst, .ops = .{ .unary = op2 } });
    self.update(branch, .{
        .dst = undefined,
        .ops = .{
            .branch = .{
                .condition = op1,
                .target = @intCast(self.code.len),
            },
        },
    });
}

// TODO: this is just plain wrong, this logic needs to be done in Ir since
// otherwise the operands are still eagerly evaluated
fn land(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);

    const op1 = self.getSlot(binary.l);
    const inv = try self.assign(inst);
    try self.add(.lnot, .{ .dst = inv, .ops = .{ .unary = op1 } });

    const zero = [_]u8{0} ** 4;
    const dst = try self.assign(inst);
    try self.add(.ld, .{ .dst = dst, .ops = .{ .imm = zero } });
    if (dead_bits & 0x1 != 0) self.unassign(binary.l);
    const branch = try self.reserve(.branch); // will consume op1

    const op2 = self.getSlot(binary.r);
    if (dead_bits & 0x2 != 0) self.unassign(binary.r);
    try self.add(.mov, .{ .dst = dst, .ops = .{ .unary = op2 } });
    self.update(branch, .{
        .dst = undefined,
        .ops = .{
            .branch = .{
                .condition = inv,
                .target = @intCast(self.code.len),
            },
        },
    });
}

const PhiContext = enum {
    branch_entry,
    branch_if,
    branch_else,
};

pub fn phiMovs(
    self: *Assembler,
    comptime context: PhiContext,
    phis: []const Ir.Index,
    assns: []const Register,
) !void {
    const ir = self.ir;
    for (phis, assns) |phi_inst, assn| {
        const payload = ir.instPayload(phi_inst);
        const src_inst = switch (ir.instTag(phi_inst)) {
            .phi_if_else => switch (context) {
                .branch_entry => continue,
                .branch_if => payload.binary.l,
                .branch_else => payload.binary.r,
            },
            .phi_entry_if => switch (context) {
                .branch_entry => payload.binary.l,
                .branch_if => payload.binary.r,
                .branch_else => continue,
            },
            .phi_entry_else => switch (context) {
                .branch_entry => payload.binary.l,
                .branch_if => continue,
                .branch_else => payload.binary.r,
            },
            else => unreachable,
        };

        const src = self.register_map.get(src_inst).?;
        try self.add(.mov, .{
            .dst = assn,
            .ops = .{ .unary = src },
        });
    }
}

fn ifElse(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const op_extra = ir.instPayload(inst).op_extra;
    const if_else = ir.extraData(Ir.Inst.IfElse, op_extra.extra);

    // load the list of exit phis for this statement
    const phis: []const Ir.Index = @ptrCast(ir.extraSlice(ir.extraData(
        Ir.Inst.ExtraSlice,
        if_else.phis,
    )));

    // and assign each to a destination register
    const scratch_top = self.scratch.items.len;
    try self.scratch.ensureUnusedCapacity(self.arena, phis.len);
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (phis) |phi_inst| {
        const register = try self.allocate();
        try self.register_map.put(self.arena, phi_inst, register);
        self.scratch.appendAssumeCapacity(@intFromEnum(register));
    }
    const assns = self.scratch.items[scratch_top..];

    // for phis that include the entry, move source data into the dest register
    try self.phiMovs(.branch_entry, phis, @ptrCast(assns));

    // load in the condition
    const condition = self.register_map.get(op_extra.op).?;
    const dead_bits = ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(condition);

    // and invert it so we can branch to false
    const inv = try self.allocate();
    try self.add(.lnot, .{
        .dst = inv,
        .ops = .{ .unary = condition },
    });
    self.deallocate(inv);

    // this branches over the if clause to the else clause, will be filled in later
    const else_branch = try self.reserve(.branch);

    // generate the "if" block and its phis
    try self.generateBlock(if_else.exec_true);
    try self.phiMovs(.branch_if, @ptrCast(phis), @ptrCast(assns));
    const else_target: u32 = @intCast(self.code.len);

    // this jumps over the else clause to the exit
    const exit_jump = try self.reserve(.jump);

    // generate the "else" block and its phis
    try self.generateBlock(if_else.exec_false);
    try self.phiMovs(.branch_else, @ptrCast(phis), @ptrCast(assns));
    const exit_target: u32 = @intCast(self.code.len);

    // now that we know the layout, go back and patch the jumps
    self.update(else_branch, .{
        .dst = undefined,
        .ops = .{
            .branch = .{
                .condition = condition,
                .target = else_target,
            },
        },
    });
    self.update(exit_jump, .{
        .dst = undefined,
        .ops = .{ .target = exit_target },
    });
}

fn loop(self: *Assembler, inst: Ir.Index) !void {
    const op_extra = self.ir.instPayload(inst).op_extra;
    const loop_data = self.ir.extraData(Ir.Inst.Loop, op_extra.extra);

    // TODO: liveness for condition
    const jump = try self.reserve(.jump);
    const body_target: u32 = @intCast(self.code.len);
    try self.generateBlock(loop_data.body);
    const condition_target: u32 = @intCast(self.code.len);
    try self.generateBlock(loop_data.condition);
    const condition = self.getSlot(op_extra.op);
    try self.add(.branch, .{
        .dst = undefined,
        .ops = .{
            .branch = .{
                .condition = condition,
                .target = body_target,
            },
        },
    });
    self.update(jump, .{
        .dst = undefined,
        .ops = .{ .target = condition_target },
    });
}
