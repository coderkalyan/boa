const std = @import("std");
const builtin = @import("builtin");
const Ir = @import("../ir/Ir.zig");
const InternPool = @import("../InternPool.zig");

register_count: u32,
code: List.Slice,

pub const List = std.MultiArrayList(Inst);
pub const Register = enum(u32) { _ };
pub const Inst = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        // load a 32 bit immediate into register
        // .imm
        ld,
        // load a wide immediate (64 bit) into register
        // .wimm,
        ldw,

        // load the value of a global variable by identifier
        // .ip
        ld_global,
        // store a value in a global variable by identifier
        // .unary_ip
        st_global,

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
        trampoline,

        // jump to index if register is true (1)
        branch,
        // unconditionally jump to index
        jump,
        exit,

        // fake instruction, stores intern pool pointer
        pool,
    };

    pub const Payload = struct {
        dst: Register,
        ops: union {
            unary: Register,
            binary: struct {
                op1: Register,
                op2: Register,
            },
            imm: [4]u8,
            wimm: [8]u8,
            branch: struct {
                condition: Register,
                target: u32,
            },
            target: u32,
            ip: InternPool.Index,
            pool: *InternPool,
            store: struct {
                ip: InternPool.Index,
                val: Register,
            },
        },

        comptime {
            if (builtin.mode != .Debug) {
                std.debug.assert(@sizeOf(Payload) <= 12);
            }
        }
    };
};
