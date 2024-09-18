const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const ShapePool = @This();

gpa: Allocator,
shapes: std.SegmentedList(Shape, 1),
descriptor_sets: std.SegmentedList(DescriptorSet, 1),

pub const Shape = struct {
    descriptor_set: DescriptorSetIndex,
    descriptor_count: u32,
    transitions: std.ArrayListUnmanaged(Transition),
};

pub const DescriptorSet = std.ArrayListUnmanaged(Descriptor);

pub const Descriptor = struct {
    name: InternPool.Index,
    type: InternPool.Index,
};

pub const Transition = struct {
    name: InternPool.Index,
    next: ShapeIndex,
};

pub const ShapeIndex = enum(u32) { _ };
pub const DescriptorSetIndex = enum(u32) { empty, _ };
pub const TransitionsIndex = enum(u32) { _ };

pub fn init(gpa: Allocator) !ShapePool {
    var pool: ShapePool = .{
        .gpa = gpa,
        .shapes = .{},
        .descriptor_sets = .{},
    };

    // add the .empty descriptor
    try pool.descriptor_sets.append(gpa, .{});

    return pool;
}

pub fn deinit(pool: *ShapePool) void {
    const gpa = pool.gpa;

    var shape_it = pool.shapes.iterator(0);
    while (shape_it.next()) |shape_ptr| shape_ptr.transitions.deinit(gpa);
    pool.shapes.deinit(gpa);

    var ds_it = pool.descriptor_sets.iterator(0);
    while (ds_it.next()) |ds| ds.deinit(gpa);
    pool.descriptor_sets.deinit(gpa);
}

pub fn createShape(pool: *ShapePool) !ShapeIndex {
    const index: u32 = @intCast(pool.shapes.count());
    try pool.shapes.append(pool.gpa, .{
        .descriptor_set = .empty,
        .descriptor_count = 0,
        .transitions = .{},
    });
    return @enumFromInt(index);
}
// pub fn createShape(pool: *ShapePool, descriptor_set: *const DescriptorSet) !ShapeIndex {
//     const index: u32 = @intCast(pool.shapes.count());
//     if (descriptor_set.items.len == 0) {
//         try pool.shapes.append(pool.gpa, .{
//             .descriptor_set = .empty,
//             .descriptor_count = 0,
//             .transitions = .{},
//         });
//     } else {
//         try pool.shapes.append(pool.gpa, .{
//             .descriptor_set = try descriptor_set.clone(pool.gpa),
//             .descriptor_count = 0,
//             .transitions = .{},
//         });
//     }
//
//     return @enumFromInt(index);
// }

pub fn shapePtr(pool: *ShapePool, index: ShapeIndex) *Shape {
    const i = @intFromEnum(index);
    return pool.shapes.at(i);
}

pub fn descriptorSetPtr(pool: *ShapePool, index: DescriptorSetIndex) *DescriptorSet {
    const i = @intFromEnum(index);
    return pool.descriptor_sets.at(i);
}

pub fn transition(
    pool: *ShapePool,
    shape: *Shape,
    name: InternPool.Index,
    ty: InternPool.Index,
) !*Shape {
    for (shape.transitions.items) |*edge| {
        if (edge.name == name) return pool.shapePtr(edge.next);
    }

    // no existing transition, so create a new map (node) and transition (edge)
    const next_index: u32 = @intCast(pool.shapes.count());
    const next_ptr = try pool.shapes.addOne(pool.gpa);
    next_ptr.descriptor_count = shape.descriptor_count + 1;
    next_ptr.transitions = .{};

    if (shape.transitions.items.len == 0 and shape.descriptor_set != .empty) {
        // if the existing shape has no transitions, we can reuse its descriptor set
        next_ptr.descriptor_set = shape.descriptor_set;
        const set_ptr = pool.descriptorSetPtr(next_ptr.descriptor_set);
        try set_ptr.append(pool.gpa, .{ .name = name, .type = ty });
    } else {
        const existing_ptr = pool.descriptorSetPtr(shape.descriptor_set);

        const count: u32 = @intCast(pool.descriptor_sets.count());
        const set_index: DescriptorSetIndex = @enumFromInt(count);
        next_ptr.descriptor_set = set_index;
        try pool.descriptor_sets.append(pool.gpa, .{});

        const set_ptr = pool.descriptorSetPtr(set_index);
        try set_ptr.ensureTotalCapacity(pool.gpa, next_ptr.descriptor_count);
        set_ptr.appendSliceAssumeCapacity(existing_ptr.items[0..shape.descriptor_count]);
        set_ptr.appendAssumeCapacity(.{ .name = name, .type = ty });
    }

    // create an edge (transition) linking the old shape to the new
    try shape.transitions.append(pool.gpa, .{
        .name = name,
        .next = @enumFromInt(next_index),
    });

    return next_ptr;
}

test "shape transitions" {
    var pool = try ShapePool.init(std.testing.allocator);
    defer pool.deinit();

    const shape0 = try pool.createShape();
    const shape0_ptr = pool.shapePtr(shape0);
    try std.testing.expectEqual(shape0, @as(ShapeIndex, @enumFromInt(0)));
    try std.testing.expectEqual(shape0_ptr.descriptor_set, DescriptorSetIndex.empty);
    try std.testing.expectEqual(0, shape0_ptr.descriptor_count);
    try std.testing.expectEqual(0, shape0_ptr.transitions.items.len);

    const foo: InternPool.Index = @enumFromInt(100);
    const shape1_ptr = try pool.transition(shape0_ptr, foo, .int);
    const shape1_ds_ptr = pool.descriptorSetPtr(shape1_ptr.descriptor_set);
    try std.testing.expectEqual(1, shape1_ptr.descriptor_count);
    try std.testing.expectEqual(1, shape1_ds_ptr.items.len);
    try std.testing.expectEqual(
        Descriptor{ .name = foo, .type = .int },
        shape1_ds_ptr.items[0],
    );
    try std.testing.expectEqual(0, shape1_ptr.transitions.items.len);
    try std.testing.expectEqual(1, shape0_ptr.transitions.items.len);
    try std.testing.expectEqual(
        Transition{ .name = foo, .next = @enumFromInt(1) },
        shape0_ptr.transitions.items[0],
    );

    const bar: InternPool.Index = @enumFromInt(101);
    const shape2_ptr = try pool.transition(shape1_ptr, bar, .float);
    const shape2_ds_ptr = pool.descriptorSetPtr(shape2_ptr.descriptor_set);
    try std.testing.expectEqual(2, shape2_ptr.descriptor_count);
    try std.testing.expectEqual(2, shape2_ds_ptr.items.len);
    try std.testing.expectEqual(
        Descriptor{ .name = foo, .type = .int },
        shape2_ds_ptr.items[0],
    );
    try std.testing.expectEqual(
        Descriptor{ .name = bar, .type = .float },
        shape2_ds_ptr.items[1],
    );
    try std.testing.expectEqual(0, shape2_ptr.transitions.items.len);
    try std.testing.expectEqual(1, shape1_ptr.transitions.items.len);
    try std.testing.expectEqual(
        Transition{ .name = bar, .next = @enumFromInt(2) },
        shape1_ptr.transitions.items[0],
    );

    try std.testing.expectEqual(shape1_ds_ptr, shape2_ds_ptr);
    try std.testing.expectEqual(shape1_ptr.descriptor_set, shape2_ptr.descriptor_set);

    const baz: InternPool.Index = @enumFromInt(102);
    const shape3_ptr = try pool.transition(shape1_ptr, baz, .bool);
    const shape3_ds_ptr = pool.descriptorSetPtr(shape3_ptr.descriptor_set);
    try std.testing.expectEqual(2, shape3_ptr.descriptor_count);
    try std.testing.expectEqual(2, shape3_ds_ptr.items.len);
    try std.testing.expectEqual(
        Descriptor{ .name = foo, .type = .int },
        shape3_ds_ptr.items[0],
    );
    try std.testing.expectEqual(
        Descriptor{ .name = baz, .type = .bool },
        shape3_ds_ptr.items[1],
    );
    try std.testing.expectEqual(0, shape3_ptr.transitions.items.len);
    try std.testing.expectEqual(2, shape1_ptr.transitions.items.len);
    try std.testing.expectEqual(
        Transition{ .name = bar, .next = @enumFromInt(2) },
        shape1_ptr.transitions.items[0],
    );
    try std.testing.expectEqual(
        Transition{ .name = baz, .next = @enumFromInt(3) },
        shape1_ptr.transitions.items[1],
    );

    try std.testing.expect(shape1_ds_ptr != shape3_ds_ptr);
    try std.testing.expect(shape1_ptr.descriptor_set != shape3_ptr.descriptor_set);

    const shape1_copy = try pool.transition(shape0_ptr, foo, .int);
    try std.testing.expectEqual(shape1_ptr, shape1_copy);
    const shape3_copy = try pool.transition(shape1_ptr, baz, .bool);
    try std.testing.expectEqual(shape3_ptr, shape3_copy);
    try std.testing.expectEqual(4, pool.shapes.count());
    try std.testing.expectEqual(3, pool.descriptor_sets.count());
}
