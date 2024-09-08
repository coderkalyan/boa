const std = @import("std");
const Bytecode = @import("../bc/Bytecode.zig");

const Allocator = std.mem.Allocator;
const Tag = Bytecode.Inst.Tag;
const Payload = Bytecode.Inst.Payload;
const asBytes = std.mem.asBytes;

const Handler = *const fn (
    in_pc: usize,
    tags: [*]const Tag,
    payload: [*]const Payload,
    stack: [*]u64,
) void;
const jump_table: [std.meta.tags(Tag).len]Handler = .{
    ld, // ld
    ldw, // ldw
    trap, // mov
    trap, // ineg
    trap, // fneg
    trap, // binv
    trap, // lnot
    iadd, // iadd
    trap, // fadd
    trap, // isub
    trap, // fsub
    imul, // imul
    trap, // fmul
    trap, // idiv
    trap, // fdiv
    trap, // imod
    trap, // fmod
    trap, // ipow
    trap, // fpow
    trap, // bor
    trap, // band
    trap, // bxor
    trap, // sll
    trap, // sra
    trap, // ieq
    trap, // ine
    trap, // ilt
    trap, // flt
    trap, // igt
    trap, // fgt
    trap, // ile
    trap, // fle
    trap, // ige
    trap, // fge
    branch, // branch
    trap, // exit
};

pub fn entry(gpa: Allocator, bc: *const Bytecode) !void {
    const stack = try gpa.alloc(u64, 4);
    @memset(stack, 0);
    defer gpa.free(stack);

    const tags = bc.code.items(.tag).ptr;
    const payloads = bc.code.items(.payload).ptr;
    entryInner(0, tags, payloads, stack.ptr);
}

fn entryInner(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    next(pc, tags, payloads, stack);
}

inline fn next(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    // std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}, ", .{ i, stack[i] });
    // std.debug.print("\n", .{});
    const handler = jump_table[@intFromEnum(tags[pc])];
    @call(.always_tail, handler, .{ pc, tags, payloads, stack });
}

inline fn readOperand(in_pc: usize, code: [*]const u8, op_width: u8) u32 {
    var operand: u32 = 0;
    for (0..op_width) |i| {
        const offset: u5 = @truncate(i);
        operand |= (@as(u32, code[in_pc + i]) << (offset * 8));
    }
    return operand;
}

fn ld(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst = payloads[pc].dst;
    const imm: u32 = @bitCast(payloads[pc].ops.imm);
    stack[@intFromEnum(dst)] = imm;
    next(pc + 1, tags, payloads, stack);
}

fn ldw(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst = payloads[pc].dst;
    const imm: u64 = @bitCast(payloads[pc].ops.wimm);
    stack[@intFromEnum(dst)] = imm;
    next(pc + 1, tags, payloads, stack);
}

fn iadd(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst] = stack[op1] + stack[op2];
    next(pc + 1, tags, payloads, stack);
}

fn isub(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst] = stack[op1] - stack[op2];
    next(pc + 1, tags, payloads, stack);
}

fn imul(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst] = stack[op1] * stack[op2];
    next(pc + 1, tags, payloads, stack);
}

fn idiv(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst] = stack[op1] / stack[op2];
    next(pc + 1, tags, payloads, stack);
}

fn branch(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    const condition: u32 = @intFromEnum(payloads[pc].ops.branch.condition);
    const branch_target: u32 = payloads[pc].ops.branch.target;
    const target = if (stack[condition] == 1) branch_target else pc + 1;
    next(target, tags, payloads, stack);
}

fn trap(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]u64) void {
    _ = pc;
    _ = tags;
    _ = payloads;
    _ = stack;

    std.debug.print("trap\n", .{});
    // while (true) {}
}
