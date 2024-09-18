const std = @import("std");
const builtin = @import("builtin");
const Ir = @import("../ir/Ir.zig");
const InternPool = @import("../InternPool.zig");

register_count: u32,
code: []const Word,

pub const List = std.ArrayListUnmanaged(Word);

pub const Opcode = enum(u32) {
    // load a 32 bit immediate into register
    // .imm
    ld,
    // load a constant pool entry into register
    // .wimm,
    ldi,

    // load the value of a global variable by identifier
    // .ip
    ldg,
    // store a value in a global variable by identifier
    // .unary_ip
    stg,

    // move from register to register
    // .unary: src register
    mov,

    // type casting
    // .unary: operand
    // int to float
    itof,
    // float to int
    ftoi,

    // unary operations
    // .unary: operand
    // integer negation
    ineg,
    // float negation
    fneg,
    // bitwise invert
    binv,
    // boolean invert
    lnot,

    // binary operations
    // .binary: (op1, op2)
    // integer add
    iadd,
    // float add
    fadd,
    // integer subtract
    isub,
    // float subtract
    fsub,
    // integer multiply
    imul,
    // float multiply
    fmul,
    // integer divide
    idiv,
    // float divide
    fdiv,
    // integer modulo
    imod,
    // float modulo
    fmod,
    // integer raise to power
    ipow,
    // float raise to power
    fpow,
    // bitwise or
    bor,
    // bitwise and
    band,
    // bitwise xor
    bxor,
    // logical shift left
    sll,
    // arithmetic shift right
    sra,
    // integer equal
    ieq,
    // integer not equal
    ine,
    // integer less than
    ilt,
    // float less than
    flt,
    // integer greater than
    igt,
    // float greater than
    fgt,
    // integer less than equal
    ile,
    // float less than equal
    fle,
    // integer greater than equal
    ige,
    // float greater than equal
    fge,

    // function call
    call,
    // schedules lazy compilation of a function and then enters it
    // currently unused
    trampoline,

    // print type
    pint,
    pfloat,
    pbool,
    pstr,

    // return the length of a string (constant time field lookup)
    strlen,
    // catenate (join) two strings
    strcat,
    // repeat a string
    strrep,

    // jump to index if register is true (1)
    branch,
    // unconditionally jump to index
    jump,
    // return from a function call
    ret,
    // exit the interpreter
    exit,
};

pub const Register = i32;

pub const Word = extern union {
    opcode: Opcode,
    register: Register,
    imm: u32,
    target: u32,
    ip: InternPool.Index,
    count: u32,

    comptime {
        if (builtin.mode != .Debug) {
            std.debug.assert(@sizeOf(Word) == 4);
        }
    }
};
