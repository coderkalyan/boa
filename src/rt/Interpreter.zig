const std = @import("std");
const Bytecode = @import("../bc/Bytecode.zig");

const Allocator = std.mem.Allocator;
const Tag = Bytecode.Inst.Tag;
const Payload = Bytecode.Inst.Payload;
const asBytes = std.mem.asBytes;

const Slot = extern union {
    int: i64,
    float: f64,
};

const Handler = *const fn (
    in_pc: usize,
    tags: [*]const Tag,
    payload: [*]const Payload,
    stack: [*]Slot,
) void;
const jump_table: [std.meta.tags(Tag).len]Handler = .{
    ld, // ld
    ldw, // ldw
    mov, // mov
    itof, // itof
    ftoi, // ftoi
    ineg, // ineg
    fneg, // fneg
    binv, // binv
    lnot, // lnot
    iadd, // iadd
    fadd, // fadd
    isub, // isub
    fsub, // fsub
    imul, // imul
    fmul, // fmul
    idiv, // idiv
    fdiv, // fdiv
    imod, // imod
    fmod, // fmod
    ipow, // ipow
    fpow, // fpow
    bor, // bor
    band, // band
    bxor, // bxor
    sll, // sll
    sra, // sra
    ieq, // ieq
    ine, // ine
    ilt, // ilt
    trap, // flt
    igt, // igt
    trap, // fgt
    ile, // ile
    trap, // fle
    ige, // ige
    trap, // fge
    branch, // branch
    jump, // jump
    trap, // exit
};

pub fn entry(gpa: Allocator, bc: *const Bytecode) !void {
    const stack = try gpa.alloc(Slot, 2000);
    @memset(stack, .{ .int = 0 });
    defer gpa.free(stack);

    const tags = bc.code.items(.tag).ptr;
    const payloads = bc.code.items(.payload).ptr;
    entryInner(0, tags, payloads, stack.ptr);
}

fn entryInner(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    next(pc, tags, payloads, stack);
}

inline fn next(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
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

fn ld(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst = payloads[pc].dst;
    const imm: u32 = @bitCast(payloads[pc].ops.imm);
    stack[@intFromEnum(dst)].int = imm;
    next(pc + 1, tags, payloads, stack);
}

fn ldw(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst = payloads[pc].dst;
    const imm: i64 = @bitCast(payloads[pc].ops.wimm);
    stack[@intFromEnum(dst)].int = imm;
    next(pc + 1, tags, payloads, stack);
}

fn mov(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = stack[op].int;
    next(pc + 1, tags, payloads, stack);
}

fn itof(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].float = @bitCast(stack[op].int);
    next(pc + 1, tags, payloads, stack);
}

fn ftoi(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = @bitCast(stack[op].float);
    next(pc + 1, tags, payloads, stack);
}

fn ineg(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = -stack[op].int;
    next(pc + 1, tags, payloads, stack);
}

fn fneg(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].float = -stack[op].float;
    next(pc + 1, tags, payloads, stack);
}

fn binv(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = ~stack[op].int;
    next(pc + 1, tags, payloads, stack);
}

fn lnot(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = @intFromBool(stack[op].int == 0);
    next(pc + 1, tags, payloads, stack);
}

fn iadd(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int + stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn fadd(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = stack[op1].float + stack[op2].float;
    next(pc + 1, tags, payloads, stack);
}

fn isub(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int - stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn fsub(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = stack[op1].float - stack[op2].float;
    next(pc + 1, tags, payloads, stack);
}

fn imul(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int * stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn fmul(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = stack[op1].float * stack[op2].float;
    next(pc + 1, tags, payloads, stack);
}

fn idiv(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int * stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn fdiv(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = stack[op1].float * stack[op2].float;
    next(pc + 1, tags, payloads, stack);
}

fn imod(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @mod(stack[op1].int, stack[op2].int); // TODO: is this correct?
    next(pc + 1, tags, payloads, stack);
}

fn fmod(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = @mod(stack[op1].float, stack[op2].float); // TODO: is this correct?
    next(pc + 1, tags, payloads, stack);
}

fn ipow(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = std.math.pow(i64, stack[op1].int, stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn fpow(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].float = std.math.pow(f64, stack[op1].float, stack[op2].float);
    next(pc + 1, tags, payloads, stack);
}

fn bor(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int | stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn band(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int & stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn bxor(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int ^ stack[op2].int;
    next(pc + 1, tags, payloads, stack);
}

fn sll(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int << @truncate(@as(u64, @intCast(stack[op2].int)));
    next(pc + 1, tags, payloads, stack);
}

fn sra(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = stack[op1].int >> @truncate(@as(u64, @bitCast(stack[op2].int)));
    next(pc + 1, tags, payloads, stack);
}

fn ieq(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int == stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn ine(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int != stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn ilt(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int < stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn igt(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int > stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn ile(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int <= stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn ige(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int >= stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn branch(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const condition: u32 = @intFromEnum(payloads[pc].ops.branch.condition);
    const branch_target: u32 = payloads[pc].ops.branch.target;
    const target = if (stack[condition].int == 1) branch_target else pc + 1;
    next(target, tags, payloads, stack);
}

fn jump(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const target: u32 = payloads[pc].ops.target;
    next(target, tags, payloads, stack);
}

fn trap(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    _ = pc;
    _ = tags;
    _ = payloads;
    // _ = stack;

    std.debug.print("trap\n", .{});
    std.debug.print("stack: ", .{});
    for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    std.debug.print("\n", .{});
    // while (true) {}
}
