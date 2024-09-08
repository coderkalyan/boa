const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");
// const RegisterAllocator = @import("RegisterAllocator.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const Inst = Ir.Inst;

const Assembler = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: std.ArrayListUnmanaged(u8),
// ra: RegisterAllocator,
// live: std.AutoArrayHashMapUnmanaged(Ir.Index, u32),
// tmax: u32,
// TODO: this can probably be faster
stack_frame: std.MultiArrayList(Slot),

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
    };
    try assembler.generateBlock(ir.block);

    return .{
        .ir = ir,
        .code = try assembler.code.toOwnedSlice(gpa),
    };
}

fn generateBlock(self: *Assembler, index: Ir.ExtraIndex) error{OutOfMemory}!void {
    const ir = self.ir;
    const block = ir.extraData(Inst.ExtraSlice, index);
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
        .alloc => try self.alloc(inst),
        .load => try self.load(inst),
        .store => try self.store(inst),
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
        // .lor,
        // .land,
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        => try self.binaryOp(inst),
        .neg, .binv, .lnot => try self.unaryOp(inst),
        .branch_double => try self.branchDouble(inst),
        // .phiarg => try self.phiarg(inst),
        else => {},
    }
}

inline fn operandWidth(operand: u32) u8 {
    if (operand <= std.math.maxInt(u8)) return 1;
    if (operand <= std.math.maxInt(u16)) return 2;
    return 4;
}

fn add(self: *Assembler, opcode: Opcode, operands: []const u32) !void {
    var width: u8 = 0;
    for (operands) |operand| width = @max(width, operandWidth(operand));

    const prefix = width > 1;
    // 1 byte for opcode, 1 byte for prefix if needed, and then width * len bytes for operand
    const bytes = @as(usize, 1) + @intFromBool(prefix) + (width * operands.len);
    try self.code.ensureUnusedCapacity(self.gpa, bytes);

    switch (width) {
        0, 1 => {},
        2 => self.code.appendAssumeCapacity(@intFromEnum(Opcode.wide)),
        4 => self.code.appendAssumeCapacity(@intFromEnum(Opcode.dwide)),
        else => unreachable,
    }

    self.code.appendAssumeCapacity(@intFromEnum(opcode));

    for (operands) |operand| {
        var shift_out = operand;
        inline for (0..4) |i| {
            if (i < width) {
                self.code.appendAssumeCapacity(@truncate(shift_out & 0xff));
                shift_out >>= 8;
            }
        }
    }
}

// TODO: perf shows that this single O(n) function is *ridiculously* slow,
// accounting for 70-80% of the entire execution time of the compiler.
// fix this to get any reasonable level of performance
fn assign(self: *Assembler, inst: Ir.Index) !u32 {
    for (self.stack_frame.items(.live), 0..) |live, i| {
        if (live) continue;
        self.stack_frame.set(i, .{ .live = true, .inst = inst });
        return @intCast(i);
    }

    try self.stack_frame.append(self.arena, .{ .live = true, .inst = inst });
    return @intCast(self.stack_frame.len - 1);
}

fn unassign(self: *Assembler, inst: Ir.Index) void {
    const i = self.getSlot(inst);
    self.stack_frame.items(.live)[i] = false;
}

fn getSlot(self: *Assembler, inst: Ir.Index) u32 {
    for (0..self.stack_frame.len) |i| {
        const slot = self.stack_frame.get(i);
        if (slot.live and slot.inst == inst) return @intCast(i);
    }

    unreachable;
}

fn constant(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const tv = self.pool.get(ip).tv;

    const immediate: u32 = switch (tv.ty) {
        .nonetype => unreachable, // TODO: this shouldn't emit any immediate, its implicit
        .int => @intCast(tv.val.int),
        .float => @bitCast(@as(f32, @floatCast(tv.val.float))),
        .bool => @intFromBool(tv.val.bool),
        else => unreachable,
    };
    const dest = try self.assign(inst);
    try self.add(.ld, &.{ dest, immediate });
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
    const opcode: Opcode = switch (ty) {
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
            .eq => .feq,
            .ne => .fne,
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
    const dest = try self.assign(inst);
    try self.add(opcode, &.{ dest, op1, op2 });
}

fn unaryOp(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const dead_bits = self.ir.liveness.deadBits(inst);
    const ty = self.pool.get(self.ir.typeOf(unary)).ty;
    const opcode: Opcode = switch (self.ir.instTag(inst)) {
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
    const dest = try self.assign(inst);
    try self.add(opcode, &.{ dest, op });
}

fn branchDouble(self: *Assembler, inst: Ir.Index) !void {
    const op_extra = self.ir.instPayload(inst).op_extra;
    const branch_double = self.ir.extraData(Inst.BranchDouble, op_extra.extra);
    // TODO: liveness for condition
    const condition = self.getSlot(op_extra.op);
    // TODO: to make this better sized, we need assembler relaxation, which
    // isn't trivial to implement
    try self.add(.btrue, &.{ condition, std.math.maxInt(u32) });
    const btrue_pc = self.code.items.len;
    try self.generateBlock(branch_double.exec_false);
    // TODO: cleaner code patching here
    const target_pc = self.code.items.len;
    var offset: u32 = @intCast(target_pc - btrue_pc);
    for (0..4) |i| {
        self.code.items[btrue_pc - 4 + i] = @truncate(offset & 0xff);
        offset >>= 8;
    }
    try self.generateBlock(branch_double.exec_true);
}

// fn phiarg(self: *Assembler, inst: Ir.Index) !void {
//
// }
