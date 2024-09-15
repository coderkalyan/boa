const std = @import("std");
const Bytecode = @import("../bc/Bytecode.zig");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const Tag = Bytecode.Inst.Tag;
const Payload = Bytecode.Inst.Payload;
const asBytes = std.mem.asBytes;
pub const GlobalMap = std.AutoHashMap(InternPool.Index, i64);

const Slot = extern union {
    int: i64,
    float: f64,
    ptr: *anyopaque,
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
    ldGlobal, // ld_global
    stGlobal, // st_global
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
    flt, // flt
    igt, // igt
    fgt, // fgt
    ile, // ile
    fle, // fle
    ige, // ige
    fge, // fge
    trap, // call
    trap, // trampoline
    branch, // branch
    jump, // jump
    exit, // exit
    trap, // pool
};

// pub fn trampoline(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, sp: [*]Slot) void {
// const tags = bc.code.items(.tag).ptr;
// const payloads = bc.code.items(.payload).ptr;
// entryInner(1, tags, payloads, stack);
// }
pub fn entry(stack: [*]Slot, bc: *const Bytecode) void {
    const tags = bc.code.items(.tag).ptr;
    const payloads = bc.code.items(.payload).ptr;
    entryInner(1, tags, payloads, stack);
}

fn entryInner(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    next(pc, tags, payloads, stack);
}

inline fn next(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    // std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}, ", .{ i, stack[i].int });
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

fn ldGlobal(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst = payloads[pc].dst;
    const ip = payloads[pc].ops.ip;
    const global_context: *GlobalMap = @alignCast(@ptrCast((stack - @as(isize, 1))[0].ptr));
    stack[@intFromEnum(dst)].int = global_context.get(ip).?;
    next(pc + 1, tags, payloads, stack);
}

fn stGlobal(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const ip = payloads[pc].ops.store.ip;
    const val = payloads[pc].ops.store.val;
    const global_context: *GlobalMap = @alignCast(@ptrCast((stack - @as(isize, 1))[0].ptr));
    global_context.put(ip, stack[@intFromEnum(val)].int) catch unreachable;
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
    stack[dst].float = @floatFromInt(stack[op].int);
    next(pc + 1, tags, payloads, stack);
}

fn ftoi(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op: u32 = @intFromEnum(payloads[pc].ops.unary);
    stack[dst].int = @intFromFloat(stack[op].float);
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

fn flt(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].float < stack[op2].float);
    next(pc + 1, tags, payloads, stack);
}

fn igt(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int > stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn fgt(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].float > stack[op2].float);
    next(pc + 1, tags, payloads, stack);
}

fn ile(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int <= stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn fle(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].float <= stack[op2].float);
    next(pc + 1, tags, payloads, stack);
}

fn ige(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].int >= stack[op2].int);
    next(pc + 1, tags, payloads, stack);
}

fn fge(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const dst: u32 = @intFromEnum(payloads[pc].dst);
    const op1: u32 = @intFromEnum(payloads[pc].ops.binary.op1);
    const op2: u32 = @intFromEnum(payloads[pc].ops.binary.op2);
    stack[dst].int = @intFromBool(stack[op1].float >= stack[op2].float);
    next(pc + 1, tags, payloads, stack);
}

fn branch(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const condition: u32 = @intFromEnum(payloads[pc].ops.branch.condition);
    if (stack[condition].int == 1) {
        const branch_target: u32 = payloads[pc].ops.branch.target;
        next(branch_target, tags, payloads, stack);
    }
    next(pc + 1, tags, payloads, stack);
}

fn jump(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    const target: u32 = payloads[pc].ops.target;
    next(target, tags, payloads, stack);
}

// fn call(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
//     const ir_data = try IrGen.generate(gpa, &pool, &tree, module_node);
//     const ir_index = try pool.createIr(ir_data);
//     const ir = pool.irPtr(ir_index);
//     next(target, tags, payloads, stack);
// }

fn exit(pc: usize, tags: [*]const Tag, payloads: [*]const Payload, stack: [*]Slot) void {
    _ = pc;
    _ = tags;
    _ = payloads;
    _ = stack;

    std.debug.print("interpreter exit\n", .{});
    // std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    // std.debug.print("\n", .{});
    // while (true) {}
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
