const std = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const InternPool = types.InternPool;
const Shape = @This();

// how to interpret the transitions pointer
mode: Mode,
// number of attributes to read from descriptors
count: u32,
// pointer to the set of attributes
descriptors: *DescriptorSet,
// pointer to either another descriptor set (for single transitions)
// or a lazily allocated arraylist of transitions
transitions: *align(transitions_align) anyopaque,

// TODO: can make this smaller
pub const DescriptorSet = std.ArrayListUnmanaged(Descriptor);
pub const Descriptor = InternPool.Index;
pub const Transitions = std.ArrayListUnmanaged(*Shape);
pub const Mode = enum(u32) { leaf, single, multi };
const transitions_align = @max(@alignOf(Transitions), @alignOf(Shape));

pub fn init(allocator: Allocator) !Shape {
    const descriptors = try allocator.create(DescriptorSet);
    errdefer allocator.destroy(descriptors);
    descriptors.* = .{};

    return .{
        .mode = .leaf,
        .descriptors = descriptors,
        .count = 0,
        .transitions = undefined,
    };
}

pub fn get(shape: *const Shape, key: InternPool.Index) ?usize {
    const descriptors = shape.descriptors.items[0..shape.count];
    for (descriptors, 0..) |descriptor, i| {
        if (key == descriptor) return i;
    }
    return null;
}

pub fn transition(shape: *Shape, allocator: Allocator, key: InternPool.Index) !*Shape {
    std.debug.assert(shape.get(key) == null);
    switch (shape.mode) {
        .leaf => {
            const next = try allocator.create(Shape);
            errdefer allocator.destroy(next);
            shape.transitions = next;
            shape.mode = .single;

            try shape.descriptors.append(allocator, key);
            next.* = .{
                .mode = .leaf,
                .descriptors = shape.descriptors,
                .count = shape.count + 1,
                .transitions = undefined,
            };
            return next;
        },
        .single => {
            var next: *Shape = @ptrCast(shape.transitions);
            if (shape.descriptors.items[shape.count] == key) return next;

            // transition from single to multi by allocating the lazy transitions list
            const transitions = try allocator.create(Transitions);
            errdefer allocator.destroy(transitions);
            transitions.* = try Transitions.initCapacity(allocator, 2);
            transitions.appendAssumeCapacity(@ptrCast(shape.transitions));
            shape.transitions = transitions;
            shape.mode = .multi;

            const descriptors = try allocator.create(DescriptorSet);
            errdefer allocator.destroy(descriptors);
            descriptors.* = try DescriptorSet.initCapacity(allocator, shape.count + 1);
            descriptors.appendSliceAssumeCapacity(shape.descriptors.items[0..shape.count]);
            descriptors.appendAssumeCapacity(key);

            next = try allocator.create(Shape);
            errdefer allocator.destroy(next);
            next.* = .{
                .mode = .leaf,
                .descriptors = descriptors,
                .count = shape.count + 1,
                .transitions = undefined,
            };
            transitions.appendAssumeCapacity(next);
            return next;
        },
        .multi => {
            const transitions: *Transitions = @ptrCast(shape.transitions);
            for (transitions.items) |next| {
                if (next.descriptors.items[shape.count] == key) return next;
            }

            // add a new transition to a new shape/descriptor set
            const descriptors = try allocator.create(DescriptorSet);
            errdefer allocator.destroy(descriptors);
            descriptors.* = try DescriptorSet.initCapacity(allocator, shape.count + 1);
            descriptors.appendSliceAssumeCapacity(shape.descriptors.items[0..shape.count]);
            descriptors.appendAssumeCapacity(key);

            const next = try allocator.create(Shape);
            errdefer allocator.destroy(next);
            next.* = .{
                .mode = .leaf,
                .descriptors = descriptors,
                .count = shape.count + 1,
                .transitions = undefined,
            };
            try transitions.append(allocator, next);
            return next;
        },
    }
}

test "empty to single" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    // define a couple of fake intern pool ids
    const apple: InternPool.Index = @enumFromInt(0);
    const banana: InternPool.Index = @enumFromInt(1);

    const first = try arena.create(Shape);
    first.* = try Shape.init(arena);
    try std.testing.expectEqual(.leaf, first.mode);
    try std.testing.expectEqual(0, first.descriptors.items.len);
    try std.testing.expectEqual(0, first.count);

    try std.testing.expectEqual(null, first.get(apple));
    try std.testing.expectEqual(null, first.get(banana));

    const second = try first.transition(arena, apple);
    try std.testing.expectEqual(.leaf, second.mode);
    try std.testing.expectEqual(1, second.descriptors.items.len);
    try std.testing.expectEqual(1, second.count);

    try std.testing.expectEqual(.single, first.mode);
    try std.testing.expectEqual(second.descriptors, first.descriptors);
    try std.testing.expectEqual(0, first.count);
    try std.testing.expectEqual(second, @as(*Shape, @ptrCast(first.transitions)));

    try std.testing.expectEqual(0, second.get(apple));
    try std.testing.expectEqual(null, second.get(banana));

    const third = try second.transition(arena, banana);
    try std.testing.expectEqual(.leaf, third.mode);
    try std.testing.expectEqual(2, third.descriptors.items.len);
    try std.testing.expectEqual(2, third.count);

    try std.testing.expectEqual(.single, second.mode);
    try std.testing.expectEqual(third.descriptors, second.descriptors);
    try std.testing.expectEqual(1, second.count);
    try std.testing.expectEqual(third, @as(*Shape, @ptrCast(second.transitions)));

    try std.testing.expectEqual(0, third.get(apple));
    try std.testing.expectEqual(1, third.get(banana));

    var replay = first.transition(arena, apple) catch return std.testing.expect(false);
    try std.testing.expectEqual(second, replay);
    replay = replay.transition(arena, banana) catch return std.testing.expect(false);
    try std.testing.expectEqual(third, replay);
}

test "single to multi" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    // define a few fake intern pool ids
    const apple: InternPool.Index = @enumFromInt(0);
    const banana: InternPool.Index = @enumFromInt(1);
    const cherry: InternPool.Index = @enumFromInt(2);

    const empty = try arena.create(Shape);
    empty.* = try Shape.init(arena);
    try std.testing.expectEqual(.leaf, empty.mode);
    try std.testing.expectEqual(0, empty.descriptors.items.len);
    try std.testing.expectEqual(0, empty.count);

    try std.testing.expectEqual(null, empty.get(apple));
    try std.testing.expectEqual(null, empty.get(banana));
    try std.testing.expectEqual(null, empty.get(cherry));

    const first = try empty.transition(arena, apple);
    try std.testing.expectEqual(.leaf, first.mode);
    try std.testing.expectEqual(1, first.descriptors.items.len);
    try std.testing.expectEqual(1, first.count);

    try std.testing.expectEqual(.single, empty.mode);
    try std.testing.expectEqual(first.descriptors, empty.descriptors);
    try std.testing.expectEqual(0, empty.count);
    try std.testing.expectEqual(first, @as(*Shape, @ptrCast(empty.transitions)));

    try std.testing.expectEqual(0, first.get(apple));
    try std.testing.expectEqual(null, first.get(banana));
    try std.testing.expectEqual(null, first.get(cherry));

    const second = try empty.transition(arena, banana);
    try std.testing.expectEqual(.leaf, second.mode);
    try std.testing.expectEqual(1, second.descriptors.items.len);
    try std.testing.expectEqual(1, second.count);

    try std.testing.expectEqual(.multi, empty.mode);
    try std.testing.expect(second.descriptors != empty.descriptors);
    try std.testing.expectEqual(0, empty.count);
    const transitions: *const Transitions = @ptrCast(empty.transitions);
    try std.testing.expectEqual(2, transitions.items.len);
    try std.testing.expectEqual(first, transitions.items[0]);
    try std.testing.expectEqual(second, transitions.items[1]);

    try std.testing.expectEqual(null, second.get(apple));
    try std.testing.expectEqual(0, second.get(banana));
    try std.testing.expectEqual(null, second.get(cherry));

    const third = try empty.transition(arena, cherry);
    try std.testing.expectEqual(.leaf, third.mode);
    try std.testing.expectEqual(1, third.descriptors.items.len);
    try std.testing.expectEqual(1, third.count);

    try std.testing.expectEqual(.multi, empty.mode);
    try std.testing.expect(third.descriptors != empty.descriptors);
    try std.testing.expectEqual(0, empty.count);
    try std.testing.expectEqual(3, transitions.items.len);
    try std.testing.expectEqual(first, transitions.items[0]);
    try std.testing.expectEqual(second, transitions.items[1]);
    try std.testing.expectEqual(third, transitions.items[2]);

    try std.testing.expectEqual(null, third.get(apple));
    try std.testing.expectEqual(null, third.get(banana));
    try std.testing.expectEqual(0, third.get(cherry));

    var replay = empty.transition(arena, apple) catch return std.testing.expect(false);
    try std.testing.expectEqual(first, replay);
    replay = empty.transition(arena, banana) catch return std.testing.expect(false);
    try std.testing.expectEqual(second, replay);
    replay = empty.transition(arena, cherry) catch return std.testing.expect(false);
    try std.testing.expectEqual(third, replay);
}
