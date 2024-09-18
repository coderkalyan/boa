const std = @import("std");
const Assembler = @import("../bc/Assembler.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const InternPool = @import("../InternPool.zig");
const IrGen = @import("../ir/IrGen.zig");
const Ir = @import("../ir/Ir.zig");
const render = @import("../render.zig");
const String = @import("string.zig").String;

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
    in_pc: usize,
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
    arg, // arg
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
    pint, // pint
    pfloat, // pfloat
    pbool, // pbool
    pstr, // pstr
    strlen, // strlen
    strcat, // strcat
    strrep, // strrep
    branch, // branch
    jump, // jump
    ret, // ret
    exit, // exit
};

pub fn entry(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    next(pc, code, fp, sp, stack);
}

inline fn next(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const opcode = code[pc].opcode;
    const handler = jump_table[@intFromEnum(opcode)];
    @call(.always_tail, handler, .{ pc, code, fp, sp, stack });
}

fn ld(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const imm = code[pc + 2].imm;
    stack[fp + dst].int = imm;
    next(pc + 3, code, fp, sp, stack);
}

fn ldi(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const ip = code[pc + 2].ip;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    switch (pool.get(ip)) {
        .ty, .ir, .bytecode => unreachable,
        .tv => |tv| switch (pool.get(tv.ty).ty) {
            .nonetype => stack[fp + dst].int = undefined,
            .int => stack[fp + dst].int = @bitCast(tv.val.int), // TODO: should this be unsigned?
            .float => stack[fp + dst].float = tv.val.float,
            .bool => stack[fp + dst].int = @intFromBool(tv.val.bool),
            .str => unreachable, // implemented in .str TODO: change this?
            .@"union", .any => unreachable,
        },
        .str => |bytes| {
            // load a string literal from the intern pool and construct a string
            // on the heap
            const str = String.init(pool.gpa, bytes) catch unreachable;
            stack[fp + dst].ptr = @constCast(str);
        },
        .function => |fi| stack[fp + dst].ptr = pool.functionPtr(fi),
    }
    next(pc + 3, code, fp, sp, stack);
}

fn ldGlobal(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const ip = code[pc + 2].ip;
    const context: *GlobalMap = @ptrCast(@alignCast(stack[fp - 1].ptr));
    stack[fp + dst].int = context.get(ip).?;
    next(pc + 3, code, fp, sp, stack);
}

fn stGlobal(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const ip = code[pc + 2].ip;
    const context: *GlobalMap = @ptrCast(@alignCast(stack[fp - 1].ptr));
    context.put(ip, stack[fp + src].int) catch unreachable;
    next(pc + 3, code, fp, sp, stack);
}

fn arg(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const i = code[pc + 2].count;
    stack[fp + dst].int = stack[fp - 5 - i - 1].int;
    next(pc + 3, code, fp, sp, stack);
}

inline fn unary(comptime opcode: Opcode) Handler {
    return struct {
        pub fn unary(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
            const dst = code[pc + 1].register;
            const src = code[pc + 2].register;

            const op = stack[fp + src];
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
        pub fn binary(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
            const dst = code[pc + 1].register;
            const src1 = code[pc + 2].register;
            const src2 = code[pc + 3].register;

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

fn strlen(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src = code[pc + 2].register;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    const str: *const String = @ptrCast(@alignCast(stack[fp + src].ptr));
    stack[fp + dst].int = @intCast(str.len);

    next(pc + 3, code, fp, sp, stack);
}

fn strcat(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src1 = code[pc + 2].register;
    const src2 = code[pc + 3].register;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    const a: *const String = @ptrCast(@alignCast(stack[fp + src1].ptr));
    const b: *const String = @ptrCast(@alignCast(stack[fp + src2].ptr));
    const str = String.catenate(pool.gpa, a, b) catch unreachable;
    stack[fp + dst].ptr = @constCast(str);

    next(pc + 4, code, fp, sp, stack);
}

fn strrep(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src1 = code[pc + 2].register;
    const src2 = code[pc + 3].register;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    const template: *const String = @ptrCast(@alignCast(stack[fp + src1].ptr));
    const count: u64 = @intCast(stack[fp + src2].int);
    const str = String.repeat(pool.gpa, template, count) catch unreachable;
    stack[fp + dst].ptr = @constCast(str);

    next(pc + 4, code, fp, sp, stack);
}

fn branch(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const condition = code[pc + 1].register;
    if (stack[fp + condition].int == 1) {
        const target = code[pc + 2].target;
        next(target, code, fp, sp, stack);
    }
    next(pc + 3, code, fp, sp, stack);
}

fn jump(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const target = code[pc + 1].target;
    next(target, code, fp, sp, stack);
}

fn call(pc: usize, code: [*]const Word, rfp: u64, rsp: u64, stack: [*]Slot) void {
    const target = code[pc + 1].target;
    const dst = code[pc + 2].register;
    const count = code[pc + 3].count;
    const fi: *FunctionInfo = @ptrCast(@alignCast(stack[rfp + target].ptr));
    const pool: *InternPool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    for (0..count) |i| {
        const reg = code[pc + 4 + i].register;
        stack[rsp + count - 1 - i].int = stack[rfp + reg].int;
    }

    stack[rsp + count + 0].int = @bitCast(pc + 4 + count);
    stack[rsp + count + 1].int = @bitCast(rfp);
    stack[rsp + count + 2].ptr = @constCast(code);
    stack[rsp + count + 3].int = dst;
    stack[rsp + count + 4].ptr = stack[rfp - 1].ptr;

    if (fi.lazy_ir == null) {
        const ir_data = IrGen.generate(.function, pool.gpa, pool, fi.tree, fi.node) catch unreachable;
        fi.lazy_ir = pool.createIr(ir_data) catch unreachable;
        // const ir = pool.irPtr(fi.lazy_ir.?);
        // {
        //     std.debug.print("ir listing for function:\n", .{});
        //     const ir_renderer = render.IrRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        //     // _ = ir_renderer;
        //     var renderer = ir_renderer.init(std.io.getStdOut().writer(), pool.gpa, ir);
        //     renderer.render() catch unreachable;
        // }
    }
    const ir = pool.irPtr(fi.lazy_ir.?);
    if (fi.lazy_bytecode == null) {
        const bc_data = Assembler.assemble(pool.gpa, pool, ir) catch unreachable;
        fi.lazy_bytecode = pool.createBytecode(bc_data) catch unreachable;
        const bc = pool.bytecodePtr(fi.lazy_bytecode.?);
        {
            std.debug.print("bytecode listing for function: {}\n", .{bc.code.len});
            const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
            // _ = bytecode_renderer;
            var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), pool.gpa, pool, bc);
            renderer.render() catch unreachable;
        }
    }
    const bc = pool.bytecodePtr(fi.lazy_bytecode.?);

    const fp = rsp + count + 5;
    const sp = fp + bc.register_count;
    next(2, bc.code.ptr, fp, sp, stack);
}

fn ret(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    _ = sp;

    const rsp = fp - 5;
    const rpc: usize = @bitCast(stack[rsp + 0].int);
    const rfp: u64 = @bitCast(stack[rsp + 1].int);
    const rcode: [*]const Word = @ptrCast(@alignCast(stack[rsp + 2].ptr));
    const dst: u64 = @bitCast(stack[rsp + 3].int);
    stack[rfp + dst].int = stack[fp + src].int;

    // std.debug.print("interpreter ret\n", .{});
    // std.debug.print("stack:\n", .{});
    // for (fp..sp) |i| std.debug.print("x{} = {}\n", .{ i - fp, stack[i].int });
    // std.debug.print("\n", .{});
    next(rpc, rcode, rfp, rsp, stack);
}

fn exit(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    _ = pc;
    _ = code;
    _ = fp;
    _ = sp;
    _ = stack;

    // std.debug.print("interpreter exit\n", .{});
    // std.debug.print("stack:\n", .{});
    // for (fp..sp) |i| std.debug.print("x{} = {}\n", .{ i - fp, stack[i].int });
    // std.debug.print("\n", .{});
    // while (true) {}
}

fn pint(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const int = stack[fp + src].int;
    std.debug.print("{}\n", .{int});

    next(pc + 2, code, fp, sp, stack);
}

fn pfloat(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const float = stack[fp + src].float;
    std.debug.print("{}\n", .{float});

    next(pc + 2, code, fp, sp, stack);
}

fn pbool(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const int = stack[fp + src].int;
    std.debug.print("{s}\n", .{if (int == 1) "True" else "False"});

    next(pc + 2, code, fp, sp, stack);
}

fn pstr(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    const str: *const String = @ptrCast(@alignCast(stack[fp + src].ptr));
    std.debug.print("{s}\n", .{str.bytes()});

    next(pc + 2, code, fp, sp, stack);
}

fn trap(pc: usize, code: [*]const Word, fp: u64, sp: u64, stack: [*]Slot) void {
    _ = sp;
    _ = fp;
    // _ = stack;

    std.debug.print("trap at {s}\n", .{@tagName(code[pc].opcode)});
    std.debug.print("stack: ", .{});
    for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    std.debug.print("\n", .{});
    // while (true) {}
}
