const std = @import("std");
const Bytecode = @import("../bc/Bytecode.zig");

const Allocator = std.mem.Allocator;
const Opcode = Bytecode.Opcode;

const Handler = *const fn (in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void;
const jump_table: [std.meta.tags(Opcode).len]Handler = .{
    trap, // wide
    trap, // dwide
    ld, // ld
    trap, // ldi
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
    trap, // lor
    trap, // land
    trap, // ieq
    trap, // feq
    trap, // ine
    trap, // fne
    trap, // ilt
    trap, // flt
    trap, // igt
    trap, // fgt
    trap, // ile
    trap, // fle
    trap, // ige
    trap, // fge
};

pub fn entry(gpa: Allocator, bc: *const Bytecode) !void {
    const stack = try gpa.alloc(u64, 4);
    @memset(stack, 0);
    defer gpa.free(stack);
    entryInner(0, bc.code.ptr, stack.ptr, undefined);
}

fn entryInner(in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void {
    _ = op_width;

    next(in_pc, code, stack);
}

inline fn next(in_pc: usize, code: [*]const u8, stack: [*]u64) void {
    var pc = in_pc;
    var width: u8 = 1;
    var opcode: Opcode = @enumFromInt(code[pc]);
    pc += 1;

    // if encountered a wide prefix, read in the real opcode
    switch (opcode) {
        .wide => {
            width = 2;
            opcode = @enumFromInt(code[pc]);
            pc += 1;
        },
        .dwide => {
            width = 4;
            opcode = @enumFromInt(code[pc]);
            pc += 1;
        },
        else => {},
    }

    // std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}, ", .{ i, stack[i] });
    // std.debug.print("\n", .{});

    const handler = jump_table[@intFromEnum(opcode)];
    @call(.always_tail, handler, .{ pc, code, stack, width });
}

inline fn readOperand(in_pc: usize, code: [*]const u8, op_width: u8) u32 {
    var operand: u32 = 0;
    for (0..op_width) |i| {
        const offset: u5 = @truncate(i);
        operand |= (@as(u32, code[in_pc + i]) << (offset * 8));
    }
    return operand;
}

fn ld(in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void {
    var pc = in_pc;
    const dest = readOperand(pc, code, op_width);
    pc += op_width;
    const imm = readOperand(pc, code, op_width);
    pc += op_width;

    // std.debug.print("ld\n", .{});
    stack[dest] = imm;

    next(pc, code, stack);
}

fn iadd(in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void {
    var pc = in_pc;
    const dest = readOperand(pc, code, op_width);
    pc += op_width;
    const src1 = readOperand(pc, code, op_width);
    pc += op_width;
    const src2 = readOperand(pc, code, op_width);
    pc += op_width;

    // std.debug.print("iadd\n", .{});
    stack[dest] = stack[src1] + stack[src2];

    next(pc, code, stack);
}

fn imul(in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void {
    var pc = in_pc;
    const dest = readOperand(pc, code, op_width);
    pc += op_width;
    const src1 = readOperand(pc, code, op_width);
    pc += op_width;
    const src2 = readOperand(pc, code, op_width);
    pc += op_width;

    // std.debug.print("iadd\n", .{});
    stack[dest] = stack[src1] * stack[src2];

    next(pc, code, stack);
}

fn trap(in_pc: usize, code: [*]const u8, stack: [*]u64, op_width: u8) void {
    _ = in_pc;
    _ = code;
    _ = stack;
    _ = op_width;

    std.debug.print("trap\n", .{});
    // while (true) {}
}
