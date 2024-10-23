const std = @import("std");
const lex = @import("lex.zig");
pub const Ast = @import("Ast.zig");
pub const parse = @import("parse.zig");
pub const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;
const Scope = @import("ir/Scope.zig");
pub const IrGen = @import("ir/IrGen.zig");
pub const Bytecode = @import("bc/Bytecode.zig");
pub const Assembler = @import("bc/Assembler.zig");
pub const render = @import("render.zig");
pub const PageBumpAllocator = @import("PageBumpAllocator.zig");
pub const Shape = @import("rt/Shape.zig");
const GarbageCollector = @import("rt/GarbageCollector.zig");
const _compile = @import("compile.zig");

pub const Lexer = lex.Lexer;
pub const compile = _compile.compile;
pub const CompilationInfo = _compile.CompilationInfo;

comptime {
    std.testing.refAllDecls(@This());
}
