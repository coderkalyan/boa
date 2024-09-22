const std = @import("std");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const InternPool = @import("InternPool.zig");
const Scope = @import("ir/Scope.zig");
const IrGen = @import("ir/IrGen.zig");
const PageBumpAllocator = @import("PageBumpAllocator.zig");
const Shape = @import("rt/Shape.zig");
const GarbageCollector = @import("rt/GarbageCollector.zig");

comptime {
    _ = lex;
    _ = parse;
    _ = InternPool;
    _ = Scope;
    _ = IrGen;
    _ = PageBumpAllocator;
    _ = Shape;
    _ = GarbageCollector;
}
