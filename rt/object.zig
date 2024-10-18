const std = @import("std");
const Shape = @import("Shape.zig");

const Allocator = std.mem.Allocator;

pub const Object = struct {
    shape: *Shape,
    attributes: std.ArrayListUnmanaged(Attribute),
    // TODO: add in-object attributes for speeeed

    const Attribute = i64;

    pub fn init(gpa: Allocator, shape: *Shape) !*Object {
        const object = try gpa.create(Object);
        object.* = .{
            .shape = shape,
            .attributes = .{},
        };

        return object;
    }
};
