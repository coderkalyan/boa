const Ir = @import("Ir.zig");

pub const IrPtr = *const anyopaque;
pub const Index = u32;
pub const PoolIndex = u32;
pub const BlockIndex = u32;

pub const Constant = extern struct {
    ip: PoolIndex,
};

pub const LdGlobal = extern struct {
    id: PoolIndex,
};

pub const StGlobal = extern struct {
    id: PoolIndex,
    val: Index,
};

pub const Arg = extern struct {
    index: u32,
};

pub const Builtin = extern struct {
    id: PoolIndex,
};

pub const Unary = extern struct {
    op: Index,
};

pub const Binary = extern struct {
    op1: Index,
    op2: Index,
};

pub const Jmp = extern struct {
    block: BlockIndex,
};

pub const Br = extern struct {
    condition: Index,
    btrue: BlockIndex,
    bfalse: BlockIndex,
};

pub const Phi = extern struct {
    ty: PoolIndex,
    op1: Index,
    block1: BlockIndex,
    op2: Index,
    block2: BlockIndex,
};
