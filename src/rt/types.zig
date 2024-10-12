const std = @import("std");
const Ast = @import("../Ast.zig");
const Ir = @import("../ir/Ir.zig");
const InternPool = @import("../InternPool.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const ConstantPool = @import("ConstantPool.zig");
const Object = @import("object.zig").Object;

const Allocator = std.mem.Allocator;

// TODO: this should be extern?
pub const Context = struct {
    ipool: *InternPool,
    global: *Object,
    gpa: Allocator,
    pba: Allocator,
    gca: Allocator,
};

pub const FunctionInfo = extern struct {
    state: State,
    node: Ast.Node.Index,
    tree: *const Ast,
    ir: *const Ir,
    bytecode: [*]const i32,

    pub const State = enum(u32) {
        lazy,
        interpreted,
        optimized_lite,
        optimized_full,
    };
};
