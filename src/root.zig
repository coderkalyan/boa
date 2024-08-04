const std = @import("std");
const lex = @import("lex.zig");
const InternPool = @import("InternPool.zig");
const Scope = @import("ir/Scope.zig");

comptime {
    _ = lex;
    _ = InternPool;
    _ = Scope;
}
