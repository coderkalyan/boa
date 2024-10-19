const Ast = @import("Ast.zig");
const Ir = @import("ir/Ir.zig");
const Bytecode = @import("bc/Bytecode.zig");

const Node = Ast.Node;

tree: *const Ast,
ir: *const Ir,
bytecode: *const Bytecode,
node: Node.Index,
state: State,

pub const State = enum(u32) {
    lazy,
    interpreted,
    optimized_lite,
    optimized_full,
};
