const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;

const Assembler = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: Bytecode.List,
slot_count: u32,
active_slots: std.AutoHashMapUnmanaged(Ir.Index, Bytecode.Register),
free_slots: std.ArrayListUnmanaged(Bytecode.Register),
// some instructions, like phis, keep track of the mapping
// from Ir index to bytecode index, so they can be updated later
inst_map: std.AutoHashMapUnmanaged(Ir.Index, u32),

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
        .slot_count = 0,
        .active_slots = .{},
        .free_slots = .{},
        .inst_map = .{},
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
        .phiarg => try self.phiarg(inst),
        .phi => try self.phi(inst),
        .loop => try self.loop(inst),
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

fn assign(self: *Assembler, inst: Ir.Index) !Bytecode.Register {
    if (self.free_slots.items.len > 0) {
        const register = self.free_slots.swapRemove(0);
        self.active_slots.putAssumeCapacity(inst, register);
        return register;
    }

    const index: u32 = self.slot_count;
    self.slot_count += 1;
    try self.free_slots.ensureUnusedCapacity(self.arena, 1);
    try self.active_slots.ensureTotalCapacity(self.arena, @intCast(self.free_slots.capacity));

    const register: Bytecode.Register = @enumFromInt(index);
    self.active_slots.putAssumeCapacity(inst, register);
    return register;
}

fn unassign(self: *Assembler, inst: Ir.Index) void {
    if (!self.active_slots.contains(inst)) std.debug.print("no: {}\n", .{@intFromEnum(inst)});
    const index = self.active_slots.get(inst).?;
    std.debug.assert(self.active_slots.remove(inst));
    self.free_slots.appendAssumeCapacity(index);
}

fn unassignRegister(self: *Assembler, index: Bytecode.Register) void {
    self.free_slots.appendAssumeCapacity(index);
}

fn getSlot(self: *Assembler, inst: Ir.Index) Bytecode.Register {
    if (!self.active_slots.contains(inst)) std.debug.print("no: {}\n", .{@intFromEnum(inst)});
    return self.active_slots.get(inst).?;
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
        const dst = try self.assign(inst);
        try self.add(.ldw, .{ .dst = dst, .ops = .{ .wimm = imm } });
    } else {
        var imm: [4]u8 = undefined;
        switch (tv.ty) {
            .nonetype => {},
            .bool => @memcpy(&imm, asBytes(&@as(u32, @intFromBool(tv.val.bool)))),
            .int => @memcpy(&imm, asBytes(&@as(u32, @truncate(tv.val.int)))),
            else => unreachable,
        }
        const dst = try self.assign(inst);
        try self.add(.ld, .{ .dst = dst, .ops = .{ .imm = imm } });
    }
}

fn itof(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const dead_bits = self.ir.liveness.deadBits(inst);
    const op = self.getSlot(unary);
    if (dead_bits & 0x1 != 0) self.unassign(unary);
    const dst = try self.assign(inst);
    try self.add(.itof, .{ .dst = dst, .ops = .{ .unary = op } });
}

fn ftoi(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const dead_bits = self.ir.liveness.deadBits(inst);
    const op = self.getSlot(unary);
    if (dead_bits & 0x1 != 0) self.unassign(unary);
    const dst = try self.assign(inst);
    try self.add(.ftoi, .{ .dst = dst, .ops = .{ .unary = op } });
}

fn binaryOp(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);
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

    const op1 = self.getSlot(binary.l);
    const op2 = self.getSlot(binary.r);
    if (dead_bits & 0x1 != 0) self.unassign(binary.l);
    if (dead_bits & 0x2 != 0) self.unassign(binary.r);
    const dst = try self.assign(inst);
    try self.add(tag, .{ .dst = dst, .ops = .{ .binary = .{ .op1 = op1, .op2 = op2 } } });
}

fn unaryOp(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const dead_bits = self.ir.liveness.deadBits(inst);
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

    const op = self.getSlot(unary);
    if (dead_bits & 0x1 != 0) self.unassign(unary);
    const dst = try self.assign(inst);
    try self.add(tag, .{ .dst = dst, .ops = .{ .unary = op } });
}

fn lor(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);

    var one: [4]u8 = undefined;
    @memcpy(asBytes(&one), asBytes(&@as(u32, 1)));
    const dst = try self.assign(inst);
    try self.add(.ld, .{ .dst = dst, .ops = .{ .imm = one } });

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

fn ifElse(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const op_extra = ir.instPayload(inst).op_extra;
    const dead_bits = ir.liveness.deadBits(inst);
    const if_else = ir.extraData(Ir.Inst.IfElse, op_extra.extra);

    // load the list of exit phis for this statement
    const phis = ir.extraSlice(ir.extraData(Ir.Inst.ExtraSlice, if_else.phis));
    // and assign each to a destination register
    const phi_assignments = try self.arena.alloc(Bytecode.Register, phis.len);
    // TODO: is there a cleaner assign that doesn't tag the inst?
    for (phi_assignments) |*assn| assn.* = try self.assign(undefined);
    // defer for (phi_assignments) |assn| self.unassignRegister(assn);

    // for phis that include the entry, move source data into the dest register
    for (phis, phi_assignments) |extra, assn| {
        const data = ir.extraData(Ir.Inst.Phi, @enumFromInt(extra));
        const src = switch (data.semantics) {
            .branch_if_else => continue,
            .branch_entry_if, .branch_entry_else => self.getSlot(data.src1),
        };
        try self.add(.mov, .{ .dst = assn, .ops = .{ .unary = src } });
    }

    const condition = self.getSlot(op_extra.op);
    if (dead_bits & 0x1 != 0) self.unassign(op_extra.op);
    const inv = try self.assign(inst);
    try self.add(.lnot, .{ .dst = inv, .ops = .{ .unary = condition } });
    self.unassignRegister(inv);
    const else_branch = try self.reserve(.branch);

    // generate the "if" block and its phis
    try self.generateBlock(if_else.exec_true);
    for (phis, phi_assignments) |extra, assn| {
        const data = ir.extraData(Ir.Inst.Phi, @enumFromInt(extra));
        const src = switch (data.semantics) {
            .branch_if_else => self.getSlot(data.src1),
            .branch_entry_if => self.getSlot(data.src2),
            .branch_entry_else => continue,
        };
        try self.add(.mov, .{ .dst = assn, .ops = .{ .unary = src } });
    }
    const exit_jump = try self.reserve(.jump);

    const else_target: u32 = @intCast(self.code.len);
    // now that we know the layout, go back and patch the else branch
    self.update(else_branch, .{
        .dst = undefined,
        .ops = .{ .branch = .{ .condition = condition, .target = else_target } },
    });

    // generate the "else" block and its phis
    try self.generateBlock(if_else.exec_false);
    for (phis, phi_assignments) |extra, assn| {
        const data = ir.extraData(Ir.Inst.Phi, @enumFromInt(extra));
        const src = switch (data.semantics) {
            .branch_if_else => self.getSlot(data.src2),
            .branch_entry_if => continue,
            .branch_entry_else => self.getSlot(data.src2),
        };
        try self.add(.mov, .{ .dst = assn, .ops = .{ .unary = src } });
    }
    const exit_target: u32 = @intCast(self.code.len);
    // now that we know the layout, go back and patch the else branch
    self.update(exit_jump, .{
        .dst = undefined,
        .ops = .{ .target = exit_target },
    });

    // TODO: this is not a good way to solve the problem
    var i = @intFromEnum(inst) + 1;
    var phi_index: u32 = 0;
    while (i < self.ir.insts.len) : (i += 1) {
        switch (self.ir.instTag(@enumFromInt(i))) {
            .phi => {
                const payload = self.ir.instPayload(@enumFromInt(i));
                if (payload.phi.op == inst) {
                    const register = phi_assignments[phi_index];
                    try self.active_slots.put(self.arena, @enumFromInt(i), register);
                    // self.unassignRegister(register);
                    phi_index += 1;
                }
            },
            else => break,
        }
    }
}

fn phiarg(self: *Assembler, inst: Ir.Index) !void {
    const mov = try self.reserve(.mov);
    try self.inst_map.put(self.arena, inst, @intFromEnum(mov));
}

fn phi(self: *Assembler, inst: Ir.Index) !void {
    _ = self;
    _ = inst;
    // const ir = self.ir;
    // const payload = ir.instPayload(inst);
    // const op = payload.phi.op;
    // const phis = switch (ir.instTag(op)) {
    //     .if_else => slice: {
    //         const if_else = ir.extraData(Ir.Inst.IfElse, ir.instPayload(op).op_extra.extra);
    //         const bounds = ir.extraData(Ir.Inst.ExtraSlice, if_else.phis);
    //         break :slice ir.extraSlice(bounds);
    //     },
    //     else => unreachable,
    // };
    //
    // const data = ir.extraData(Ir.Inst.Phi, @enumFromInt(phis[payload.phi.index]));
    // try self.active_slots.put(self.arena, inst, register);
    // const binary = self.ir.instPayload(inst).binary;
    // const dead_bits = self.ir.liveness.deadBits(inst);
    // const src1 = self.ir.instPayload(binary.l).unary;
    // const src2 = self.ir.instPayload(binary.r).unary;
    //
    // const op1 = self.getSlot(src1);
    // const op2 = self.getSlot(src2);
    // if (dead_bits & 0x1 != 0) self.unassign(src1);
    // if (dead_bits & 0x2 != 0) self.unassign(src2);
    // const dst = try self.assign(inst);
    //
    // const arg1 = self.inst_map.get(binary.l).?;
    // const arg2 = self.inst_map.get(binary.r).?;
    // self.update(@enumFromInt(arg1), .{ .dst = dst, .ops = .{ .unary = op1 } });
    // self.update(@enumFromInt(arg2), .{ .dst = dst, .ops = .{ .unary = op2 } });
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
