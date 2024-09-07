const std = @import("std");
const Ir = @import("../ir/Ir.zig");

ir: *const Ir,
code: []const u8,

// these opcodes are used in the "assembled" representation, see Inst for
// the expanded version
pub const Opcode = enum(u8) {
    // these special opcodes mean that the operand to the following bytecode
    // should be treated as wide (more than 1 byte, which is the default)
    // they are reserve here to distinguish them from normal opcodes, but are
    // emitted automatically by the bytecode assembler (not managed manually)
    wide, // 16 bit operand
    dwide, // 32 bit operand

    // load an immediate into register
    // op1: register
    // op2: immediate
    ld,

    // load indirect into register
    // op1: register
    // op2: intern pool index
    ldi,

    // move from register to register
    // op1: dest register
    // op2: src register
    mov,
    // load from register into accumulator
    // operand: register
    // ldr,
    // store from accumulator into register
    // operand: register
    // str,

    // integer negation of accumulator
    // no operand
    ineg,
    // float negation of accumulator
    // no operand
    fneg,
    // bitwise invert of accumulator
    // no operand
    binv,
    // boolean invert of accumulator
    // no operand
    lnot,

    // integer add register to accumulator
    // operand: register
    iadd,
    // float add register to accumulator
    // operand: register
    fadd,
    // integer subtract register from accumulator
    // operand: register
    isub,
    // float subtract register from accumulator
    // operand: register
    fsub,
    // integer multiply register with accumulator
    // operand: register
    imul,
    // float multiply register with accumulator
    // operand: register
    fmul,
    // integer divide accumulator by register
    // operand: register
    idiv,
    // float divide accumulator by register
    // operand: register
    fdiv,
    // integer modulo accumulator by register
    // operand: register
    imod,
    // float modulo accumulator by register
    // operand: register
    fmod,
    // integer raise accumulator to register power
    // operand: register
    ipow,
    // float raise accumulator to register power
    // operand: register
    fpow,
    // bitwise or register with accumulator
    // operand: register
    bor,
    // bitwise and register with accumulator
    // operand: register
    band,
    // bitwise xor register with accumulator
    // operand: register
    bxor,
    // logical shift left
    sll,
    // arithmetic shift right
    sra,
    // boolean or register with accumulator
    // operand: register
    lor,
    // boolean and register with accumulator
    // operand: register
    land,

    // equal
    // operand: register
    ieq,
    feq,
    // not equal
    // operand: register
    ine,
    fne,
    // less than
    // operand: register
    ilt,
    flt,
    // greater than
    // operand: register
    igt,
    fgt,
    // less than equal
    // operand: register
    ile,
    fle,
    // greater than equal
    // operand: register
    ige,
    fge,
};
