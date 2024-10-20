const std = @import("std");

pub const types = @import("types.zig");
pub const builtins = @import("builtins.zig");
pub const Object = @import("object.zig").Object;
pub const Shape = @import("Shape.zig");

comptime {
    std.testing.refAllDecls(@This());
}
