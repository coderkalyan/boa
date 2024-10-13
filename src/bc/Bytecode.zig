const std = @import("std");

frame_size: u32,
code: []const i32,

pub const Opcode = enum(i32) {
    // load a 32 bit immediate into register
    ld,
    // load a 64 bit immediate into register
    ldw,

    // int to float
    itof,
    // float to int
    ftoi,

    // load an attribute from the global context, using an intern pool
    // index (attribute name) as an opaque key
    //
    // calls runtime to query index of attribute, stores index
    // and cache key (object shape) inline
    ldg_init,
    // guards against cache key, if incorrect resets to ldg_init
    // else returns property access on object
    ldg_fast,
    // store a value to the global context - similar construction to above
    stg_init,
    stg_fast,

    // move (copy) register
    mov,
    // integer negation
    ineg,
    // float negation
    fneg,
    // bitwise invert
    binv,
    // boolean invert
    lnot,

    // binary operations
    iadd,
    isub,
    imul,
    idiv,
    imod,

    fadd,
    fsub,
    fmul,
    fdiv,
    fmod,

    bor,
    band,
    bxor,
    sll,
    sra,

    ieq,
    ine,
    ilt,
    igt,
    ile,
    ige,

    flt,
    fgt,
    fle,
    fge,

    // push a single argument in preparation of function or runtime call
    push_one,
    // push multiple arguments
    push_multi,
    // pop a single argument
    pop_one,
    // pop multiple arguments
    pop_multi,

    // python function call - multiple instructions here to track inline caching
    //
    // when executed, will dispatch runtime to check if function has not been compiled
    // yet, and if not, compile IR and bytecode
    // it will then update its own opcode to .call and run that, which avoids the compile
    // guard on subsequent calls
    call_init,
    // call a function using the interpreter (fast path)
    call_fast,
    // runtime call
    callrt,

    // jump by offset if register is true
    br,
    // unconditionally jump to index
    jmp,
    // return from a function call
    ret,
    // exit the interpreter
    exit,
    // trap the interpreter
    trap,
};
