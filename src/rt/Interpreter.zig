const std = @import("std");
const Assembler = @import("../bc/Assembler.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const InternPool = @import("../InternPool.zig");
const IrGen = @import("../ir/IrGen.zig");
const Ir = @import("../ir/Ir.zig");
const render = @import("../render.zig");

const Allocator = std.mem.Allocator;
const Word = Bytecode.Word;
const Opcode = Bytecode.Opcode;
const asBytes = std.mem.asBytes;
pub const GlobalMap = std.AutoHashMap(InternPool.Index, i64);
const FunctionInfo = InternPool.FunctionInfo;

pub const Slot = extern union {
    int: i64,
    float: f64,
    ptr: *anyopaque,
};

const Handler = *const fn (
    in_pc: u64,
    code: [*]const Word,
    fp: u64,
    sp: u64,
    stack: [*]Slot,
) void;
const jump_table: [std.meta.tags(Opcode).len]Handler = .{
    ld, // ld
    ldi, // ldi
    ldGlobal, // ld_global
    stGlobal, // st_global
    unary(.mov), // mov
    unary(.itof), // itof
    unary(.ftoi), // ftoi
    unary(.ineg), // ineg
    unary(.fneg), // fneg
    unary(.binv), // binv
    unary(.lnot), // lnot
    binary(.iadd), // iadd
    binary(.fadd), // fadd
    binary(.isub), // isub
    binary(.fsub), // fsub
    binary(.imul), // imul
    binary(.fmul), // fmul
    binary(.idiv), // idiv
    binary(.fdiv), // fdiv
    binary(.imod), // imod
    binary(.fmod), // fmod
    binary(.ipow), // ipow
    binary(.fpow), // fpow
    binary(.bor), // bor
    binary(.band), // band
    binary(.bxor), // bxor
    binary(.sll), // sll
    binary(.sra), // sra
    binary(.ieq), // ieq
    binary(.ine), // ine
    binary(.ilt), // ilt
    binary(.flt), // flt
    binary(.igt), // igt
    binary(.fgt), // fgt
    binary(.ile), // ile
    binary(.fle), // fle
    binary(.ige), // ige
    binary(.fge), // fge
    call, // call
    trap, // trampoline
    branch, // branch
    jump, // jump
    ret, // ret
    exit, // exit
};

// pub fn trampoline(pc: u64, tags: [*]const Tag, payloads: [*]const Payload, sp: [*]Slot) void {
// const tags = bc.code.items(.tag).ptr;
// const payloads = bc.code.items(.payload).ptr;
// entryInner(1, tags, payloads, stack);
// }
// pub fn entry(pc: u64, stack: [*]Slot, bc: *const Bytecode) void {
//     entryInner(pc, bc.code.ptr, 3, 3, stack);
// }

pub fn entry(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    next(pc, code, fp, sp, stack);
}

inline fn next(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const opcode = code[pc].opcode;
    const handler = jump_table[@intFromEnum(opcode)];
    @call(.always_tail, handler, .{ pc, code, fp, sp, stack });
}

fn ld(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = @intFromEnum(code[pc + 1].register);
    const imm = code[pc + 2].imm;
    stack[fp + dst].int = imm;
    next(pc + 3, code, fp, sp, stack);
}

fn ldi(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = @intFromEnum(code[pc + 1].register);
    const ip = code[pc + 2].ip;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    switch (pool.get(ip)) {
        .ty, .ir, .bytecode => unreachable,
        .tv => unreachable, // TODO: implement
        .str => unreachable, // TODO: implement
        .function => |fi| stack[fp + dst].ptr = pool.functionPtr(fi),
    }
    next(pc + 3, code, fp, sp, stack);
}

fn ldGlobal(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = @intFromEnum(code[pc + 1].register);
    const ip = code[pc + 2].ip;
    const context: *GlobalMap = @ptrCast(@alignCast(stack[fp - 1].ptr));
    stack[fp + dst].int = context.get(ip).?;
    next(pc + 3, code, fp, sp, stack);
}

fn stGlobal(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = @intFromEnum(code[pc + 1].register);
    const ip = code[pc + 2].ip;
    const context: *GlobalMap = @ptrCast(@alignCast(stack[fp - 1].ptr));
    context.put(ip, stack[fp + src].int) catch unreachable;
    next(pc + 3, code, fp, sp, stack);
}

inline fn unary(comptime opcode: Opcode) Handler {
    return struct {
        pub fn unary(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
            const dst: u32 = @intFromEnum(code[pc + 1].register);
            const src: u32 = @intFromEnum(code[pc + 2].register);

            const op = stack[src];
            stack[fp + dst] = switch (opcode) {
                .mov => op,
                .itof => .{ .float = @floatFromInt(op.int) },
                .ftoi => .{ .int = @intFromFloat(op.float) },
                .ineg => .{ .int = -op.int },
                .fneg => .{ .float = -op.float },
                .binv => .{ .int = ~op.int },
                .lnot => .{ .int = @intFromBool(op.int == 0) },
                else => unreachable,
            };

            next(pc + 3, code, fp, sp, stack);
        }
    }.unary;
}

inline fn binary(comptime opcode: Opcode) Handler {
    return struct {
        pub fn binary(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
            const dst: u32 = @intFromEnum(code[pc + 1].register);
            const src1: u32 = @intFromEnum(code[pc + 2].register);
            const src2: u32 = @intFromEnum(code[pc + 3].register);

            const op1 = stack[fp + src1];
            const op2 = stack[fp + src2];
            stack[fp + dst] = switch (opcode) {
                .iadd => .{ .int = op1.int + op2.int },
                .fadd => .{ .float = op1.float + op2.float },
                .isub => .{ .int = op1.int - op2.int },
                .fsub => .{ .float = op1.float - op2.float },
                .imul => .{ .int = op1.int * op2.int },
                .fmul => .{ .float = op1.float * op2.float },
                .idiv => .{ .int = @divFloor(op1.int, op2.int) },
                .fdiv => .{ .float = op1.float / op2.float },
                .imod => .{ .int = @mod(op1.int, op2.int) },
                .fmod => .{ .float = @mod(op1.float, op2.float) },
                .ipow => .{ .int = std.math.pow(i64, op1.int, op2.int) },
                .fpow => .{ .float = std.math.pow(f64, op1.float, op2.float) },
                .bor => .{ .int = op1.int | op2.int },
                .band => .{ .int = op1.int & op2.int },
                .bxor => .{ .int = op1.int ^ op2.int },
                .sll => .{ .int = op1.int << @truncate(@as(u64, @bitCast(op2.int))) },
                .sra => .{ .int = op1.int >> @truncate(@as(u64, @bitCast(op2.int))) },
                .ieq => .{ .int = @intFromBool(op1.int == op2.int) },
                .ine => .{ .int = @intFromBool(op1.int != op2.int) },
                .ilt => .{ .int = @intFromBool(op1.int < op2.int) },
                .flt => .{ .int = @intFromBool(op1.float < op2.float) },
                .igt => .{ .int = @intFromBool(op1.int > op2.int) },
                .fgt => .{ .int = @intFromBool(op1.float > op2.float) },
                .ile => .{ .int = @intFromBool(op1.int <= op2.int) },
                .fle => .{ .int = @intFromBool(op1.float <= op2.float) },
                .ige => .{ .int = @intFromBool(op1.int >= op2.int) },
                .fge => .{ .int = @intFromBool(op1.float >= op2.float) },
                else => unreachable,
            };

            next(pc + 4, code, fp, sp, stack);
        }
    }.binary;
}

fn branch(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const condition = @intFromEnum(code[pc + 1].register);
    if (stack[fp + condition].int == 1) {
        const target = code[pc + 2].target;
        next(target, code, fp, sp, stack);
    }
    next(pc + 2, code, fp, sp, stack);
}

fn jump(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const target = code[pc + 1].target;
    next(target, code, fp, sp, stack);
}

fn call(pc: u64, code: [*]const Word, rfp: u64, rsp: u64, stack: [*]Slot) void {
    const target = code[pc + 1].target;
    const fi: *FunctionInfo = @ptrCast(@alignCast(stack[rfp + target].ptr));
    const pool: *InternPool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    stack[rsp + 0].int = @bitCast(pc + 2);
    stack[rsp + 1].int = @bitCast(rfp);
    stack[rsp + 2].ptr = @constCast(code);
    stack[rsp + 3].ptr = stack[rfp - 1].ptr;

    if (fi.lazy_ir == null) {
        const ir_data = IrGen.generate(.function, pool.gpa, pool, fi.tree, fi.node) catch unreachable;
        fi.lazy_ir = pool.createIr(ir_data) catch unreachable;
    }
    const ir = pool.irPtr(fi.lazy_ir.?);
    if (fi.lazy_bytecode == null) {
        const bc_data = Assembler.assemble(pool.gpa, pool, ir) catch unreachable;
        fi.lazy_bytecode = pool.createBytecode(bc_data) catch unreachable;
    }
    const bc = pool.bytecodePtr(fi.lazy_bytecode.?);
    // {
    //     std.debug.print("bytecode listing for function:\n", .{});
    //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
    //     // _ = bytecode_renderer;
    //     var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), pool.gpa, pool, bc);
    //     renderer.render() catch unreachable;
    // }

    const fp = rsp + 4;
    const sp = fp + bc.register_count;
    std.debug.print("calling\n", .{});
    next(2, bc.code.ptr, fp, sp, stack);
}

fn ret(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    _ = pc;
    _ = code;
    _ = sp;

    const rsp = fp - 4;
    const rfp: u64 = @bitCast(stack[rsp + 1].int);
    const rpc: u64 = @bitCast(stack[rsp + 0].int);
    const rcode: [*]const Word = @ptrCast(@alignCast(stack[rsp + 2].ptr));

    std.debug.print("interpreter ret\n", .{});
    next(rpc, rcode, rfp, rsp, stack);
}

fn exit(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    _ = pc;
    _ = code;
    _ = fp;
    _ = sp;
    _ = stack;

    std.debug.print("interpreter exit\n", .{});
    // std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    // std.debug.print("\n", .{});
    // while (true) {}
}

fn trap(pc: u64, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    _ = sp;
    _ = fp;
    // _ = stack;

    std.debug.print("trap at {s}\n", .{@tagName(code[pc].opcode)});
    std.debug.print("stack: ", .{});
    for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    std.debug.print("\n", .{});
    // while (true) {}
}
