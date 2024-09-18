const std = @import("std");
const ShapePool = @import("ShapePool.zig");

const Allocator = std.mem.Allocator;
const Shape = ShapePool.Shape;

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
