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
    pool.shapes.deinit(gpa);
    pool.descriptor_sets.deinit(gpa);
}

pub fn createShape(pool: *ShapePool, descriptor_set: *const DescriptorSet) !ShapeIndex {
    const index: u32 = @intCast(pool.shapes.count());
    if (descriptor_set.items.len == 0) {
        try pool.shapes.append(pool.gpa, .{
            .descriptor_set = .empty,
            .descriptor_count = 0,
            .transitions = .{},
        });
    } else {
        try pool.shapes.append(pool.gpa, .{
            .descriptor_set = try descriptor_set.clone(pool.gpa),
            .descriptor_count = 0,
            .transitions = .{},
        });
    }

    return @enumFromInt(index);
}

pub fn shapePtr(pool: *ShapePool, index: ShapeIndex) *Shape {
    const i = @intFromEnum(index);
    return pool.shapes.at(i);
}

pub fn descriptorSetPtr(pool: *ShapePool, index: DescriptorSetIndex) *Shape {
    const i = @intFromEnum(index);
    return pool.descriptors.at(i);
}

pub fn transition(
    pool: *ShapePool,
    shape: *Shape,
    name: InternPool.Index,
    ty: InternPool.Index,
) !*Shape {
    for (shape.transitions.items) |*edge| {
        if (edge.name == name) return edge.next;
    }

    // no existing transition, so create a new map (node) and transition (edge)
    const next_index: u32 = @intCast(pool.shapes.count());
    const next_ptr = try pool.shapes.addOne(pool.gpa);
    next_ptr.descriptor_count = shape.descriptor_count + 1;
    next_ptr.transitions = .{};

    // create an edge (transition) linking the old shape to the new
    try shape.transitions.append(pool.gpa, .{
        .name = name,
        .next = @enumFromInt(next_index),
    });

    if (shape.transitions.items.len == 0) {
        // if the existing shape has no transitions, we can reuse its descriptor set
        next_ptr.descriptor_set = shape.descriptor_set;
    } else {
        const existing_ptr = try pool.descriptor_sets.at(shape.descriptor_set);

        const set_index: u32 = @intCast(pool.descriptor_sets.count());
        next_ptr.descriptor_set = @enumFromInt(set_index);

        const set_ptr = try pool.descriptor_sets.append(.{});
        try set_ptr.ensureTotalCapacity(existing_ptr.items.len + 1);
        set_ptr.appendSliceAssumeCapacity(existing_ptr.items);
        set_ptr.appendAssumeCapacity(.{ .name = name, .type = ty });
    }
}

test "shape transitions" {
    var pool = try ShapePool.init(std.testing.allocator);
    defer pool.deinit();

    const shape0 = try pool.createShape(&.{});
    const shape_ptr = try pool.shapePtr(shape0);
    try std.testing.expectEqual(shape0, @as(ShapeIndex, @enumFromInt(0)));
    try std.testing.expectEqual(shape_ptr.descriptor_set, DescriptorSet.empty);
    try std.testing.expectEqual(shape_ptr.descriptor_count, 0);
    try std.testing.expectEqual(shape_ptr.transitions.items.len, 0);
}
