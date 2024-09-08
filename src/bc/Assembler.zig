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
// TODO: this can probably be faster
stack_frame: std.MultiArrayList(Slot),
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
        .stack_frame = .{},
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

    const index = @intFromEnum(inst);
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
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
        .neg,
        .binv,
        .lnot,
        => try self.unaryOp(inst),
        .branch_double => try self.branchDouble(inst),
        .phiarg => try self.phiarg(inst),
        .phi => try self.phi(inst),
        else => {},
    }
}

inline fn add(self: *Assembler, tag: Bytecode.Inst.Tag, payload: Bytecode.Inst.Payload) !void {
    try self.code.append(self.gpa, .{ .tag = tag, .payload = payload });
}

inline fn reserve(self: *Assembler, tag: Bytecode.Inst.Tag) !Bytecode.Index {
    const index: u32 = @intCast(self.code.len);
    try self.code.append(self.gpa, .{ .tag = tag, .payload = undefined });
    return @enumFromInt(index);
}

inline fn update(self: *Assembler, inst: Bytecode.Index, payload: Bytecode.Inst.Payload) void {
    const i = @intFromEnum(inst);
    self.code.items(.payload)[i] = payload;
}

// TODO: perf shows that this single O(n) function is *ridiculously* slow,
// accounting for 70-80% of the entire execution time of the compiler.
// fix this to get any reasonable level of performance
fn assign(self: *Assembler, inst: Ir.Index) !Bytecode.Index {
    for (self.stack_frame.items(.live), 0..) |live, i| {
        if (live) continue;
        self.stack_frame.set(i, .{ .live = true, .inst = inst });
        const index: u32 = @intCast(i);
        return @enumFromInt(index);
    }

    try self.stack_frame.append(self.arena, .{ .live = true, .inst = inst });
    const index: u32 = @intCast(self.stack_frame.len - 1);
    return @enumFromInt(index);
}

fn unassign(self: *Assembler, inst: Ir.Index) void {
    const i = self.getSlot(inst);
    self.stack_frame.items(.live)[@intFromEnum(i)] = false;
}

fn getSlot(self: *Assembler, inst: Ir.Index) Bytecode.Index {
    for (0..self.stack_frame.len) |i| {
        const slot = self.stack_frame.get(i);
        if (slot.live and slot.inst == inst) return @enumFromInt(@as(u32, @intCast(i)));
    }

    unreachable;
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

fn alloc(self: *Assembler, inst: Ir.Index) !void {
    _ = try self.assign(inst);
}

fn dealloc(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    self.ra.free(unary);
}

fn load(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const dead_bits = self.ir.liveness.deadBits(inst);

    const src = self.getSlot(unary);
    const dest = try self.assign(inst);
    if (dead_bits & 0x1 != 0) self.unassign(unary);
    try self.add(.mov, &.{ dest, src });
}

fn store(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);

    const dest = self.getSlot(binary.l);
    const src = self.getSlot(binary.r);
    if (dead_bits & 0x1 != 0) self.unassign(binary.l);
    if (dead_bits & 0x2 != 0) self.unassign(binary.r);
    try self.add(.mov, &.{ dest, src });
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

fn branchDouble(self: *Assembler, inst: Ir.Index) !void {
    const op_extra = self.ir.instPayload(inst).op_extra;
    const branch_double = self.ir.extraData(Ir.Inst.BranchDouble, op_extra.extra);

    // TODO: liveness for condition
    const condition = self.getSlot(op_extra.op);
    const branch = try self.reserve(.branch);

    try self.generateBlock(branch_double.exec_false);
    const target: u32 = @intCast(self.code.len);
    try self.generateBlock(branch_double.exec_true);
    self.update(branch, .{
        .dst = undefined,
        .ops = .{ .branch = .{ .condition = condition, .target = target } },
    });
}

fn phiarg(self: *Assembler, inst: Ir.Index) !void {
    const mov = try self.reserve(.mov);
    try self.inst_map.put(self.arena, inst, @intFromEnum(mov));
}

fn phi(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dead_bits = self.ir.liveness.deadBits(inst);
    const src1 = self.ir.instPayload(binary.l).unary;
    const src2 = self.ir.instPayload(binary.r).unary;

    const op1 = self.getSlot(src1);
    const op2 = self.getSlot(src2);
    if (dead_bits & 0x1 != 0) self.unassign(src1);
    if (dead_bits & 0x2 != 0) self.unassign(src2);
    const dst = try self.assign(inst);

    const arg1 = self.inst_map.get(binary.l).?;
    const arg2 = self.inst_map.get(binary.r).?;
    self.update(@enumFromInt(arg1), .{ .dst = dst, .ops = .{ .unary = op1 } });
    self.update(@enumFromInt(arg2), .{ .dst = dst, .ops = .{ .unary = op2 } });
}
