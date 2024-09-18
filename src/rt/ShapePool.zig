const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const ShapePool = @This();

gpa: Allocator,
shapes: std.SegmentedList(Shape, 0),
shapes_free_list: std.ArrayListUnmanaged(ShapeIndex),
descriptors: std.SegmentedList(Descriptor, 0),
descriptors_free_list: std.ArrayListUnmanaged(DescriptorIndex),
transitions: std.SegmentedList(Transitions, 0),
transitions_free_list: std.ArrayListUnmanaged(TransitionsIndex),

pub const Shape = struct {
    descriptor: DescriptorIndex,
    member_count: u32,
    transitions: TransitionsIndex,
};

pub const Descriptor = std.ArrayListUnmanaged(InternPool.Index);
pub const Transitions = std.ArrayListUnmanaged(Transition);

pub const Transition = struct {
    member: InternPool.Index,
    next: ShapeIndex,
};

pub const ShapeIndex = enum(u32) { _ };
pub const DescriptorIndex = enum(u32) { empty, _ };
pub const TransitionsIndex = enum(u32) { _ };

pub fn init(gpa: Allocator) !ShapePool {
    var pool: ShapePool = .{
        .gpa = gpa,
        .shapes = .{},
        .shapes_free_list = .{},
        .descriptors = .{},
        .descriptors_free_list = .{},
        .transitions = .{},
        .transitions_free_list = .{},
    };

    // add the .empty descriptor
    const descriptor = try pool.createDescriptor();
    std.debug.assert(descriptor == .empty);

    return pool;
}

pub fn deinit(pool: *ShapePool) void {
    const gpa = pool.gpa;
    pool.shapes.deinit(gpa);
    pool.shapes_free_list.deinit(gpa);
    pool.descriptors.deinit(gpa);
    pool.descriptors_free_list.deinit(gpa);
    pool.transitions.deinit(gpa);
    pool.transitions_free_list.deinit(gpa);
}

fn createShapeAssumeCapacity(
    pool: *ShapePool,
    descriptor: DescriptorIndex,
    transitions: TransitionsIndex,
) ShapeIndex {
    const index = pool.shapes_free_list.pop();
    const shape = pool.shapes.at(@intFromEnum(index));
    shape.* = .{ .descriptor = descriptor, .member_count = 0, .transitions = transitions };
    return index;
}

pub fn createShape(
    pool: *ShapePool,
    descriptor: DescriptorIndex,
    transitions: TransitionsIndex,
) !ShapeIndex {
    // if out of space, allocate a new shape
    if (pool.shapes_free_list.items.len == 0) {
        const index: u32 = @intCast(pool.shapes.count());
        _ = try pool.shapes.addOne(pool.gpa);
        try pool.shapes_free_list.append(pool.gpa, @enumFromInt(index));
    }

    return pool.createShapeAssumeCapacity(descriptor, transitions);
}

pub fn shapePtr(pool: *ShapePool, index: ShapeIndex) *Shape {
    const i = @intFromEnum(index);
    return pool.shapes.at(i);
}

fn createDescriptorAssumeCapacity(
    pool: *ShapePool,
) DescriptorIndex {
    const index = pool.descriptors_free_list.pop();
    const descriptor = pool.descriptors.at(@intFromEnum(index));
    descriptor.* = .{};
    return index;
}

pub fn createDescriptor(
    pool: *ShapePool,
) !DescriptorIndex {
    // if out of space, allocate a new shape
    if (pool.descriptors_free_list.items.len == 0) {
        const index: u32 = @intCast(pool.descriptors.count());
        _ = try pool.descriptors.addOne(pool.gpa);
        try pool.descriptors_free_list.append(pool.gpa, @enumFromInt(index));
    }

    return pool.createDescriptorAssumeCapacity();
}

pub fn descriptorPtr(pool: *ShapePool, index: ShapeIndex) *Shape {
    const i = @intFromEnum(index);
    return pool.shapes.at(i);
}
