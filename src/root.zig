const std = @import("std");
const lex = @import("lex.zig");
const InternPool = @import("InternPool.zig");

comptime {
    _ = lex;
    _ = InternPool;
}
