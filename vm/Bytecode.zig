pub const Opcode = enum(u32) {
    // load a 32 bit immediate into register
    ld,
    // load a 64 bit immediate into register
    ldw,

    // int to float
    // itof,
    // float to int
    // ftoi,

    // move (copy) register
    mov,
    // integer negation
    // ineg,
    // float negation
    // fneg,
    // bitwise invert
    // binv,
    // boolean invert
    // lnot,

    // binary operations
    // iadd,
    // isub,
    // imul,
    // idiv,
    // imod,
    //
    // fadd,
    // fsub,
    // fmul,
    // fdiv,
    // fmod,
    //
    // bor,
    // band,
    // bxor,
    // sll,
    // sra,
    //
    // ieq,
    // ine,
    // ilt,
    // igt,
    // ile,
    // ige,
    //
    // flt,
    // fgt,
    // fle,
    // fge,

    // push a single argument in preparation of function or runtime call
    // push,
    // push multiple arguments
    // push_multi,

    // python function call
    // call,
    // runtime call
    // callrt,

    // jump by offset if register is true
    // br,
    // unconditionally jump to index
    // jmp,
    // return from a function call
    // ret,
    // exit the interpreter
    exit,
    // trap the interpreter
    trap,
};
