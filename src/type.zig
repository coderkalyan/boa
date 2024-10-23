const std = @import("std");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const Item = InternPool.Item;
const asBytes = std.mem.asBytes;

pub const Type = union(enum) {
    nonetype: void,
    int: void,
    float: void,
    bool: void,
    str: void,
    list: InternPool.Index,
    @"union": []const InternPool.Index,
    object: []const Attribute,
    any: void,

    pub const Tag = std.meta.Tag(Type);
    pub const Attribute = struct {
        name: InternPool.Index,
        ty: InternPool.Index,
    };

    // converts a Type into an InternPool.Item for efficient
    // storage. Extra serialization storage is available via
    // the .extra and .wide arrays owned by the pool.
    pub fn serialize(ty: Type, pool: *InternPool) !Item {
        return switch (ty) {
            .nonetype => .{ .tag = .nonetype_ty, .payload = .{ .placeholder = {} } },
            .int => .{ .tag = .int_ty, .payload = .{ .placeholder = {} } },
            .float => .{ .tag = .float_ty, .payload = .{ .placeholder = {} } },
            .bool => .{ .tag = .bool_ty, .payload = .{ .placeholder = {} } },
            .str => .{ .tag = .str_ty, .payload = .{ .placeholder = {} } },
            .list => |ip| .{ .tag = .list_ty, .payload = .{ .ip = ip } },
            .@"union" => |types| {
                const slice = try pool.addSlice(@ptrCast(types));
                return .{ .tag = .union_ty, .payload = .{ .extra = slice } };
            },
            .object => |fields| {
                const ptr = asBytes(fields.ptr);
                comptime std.debug.assert(@alignOf(InternPool.Index) == @alignOf(Attribute));
                const ips: [*]const InternPool.Index = @ptrCast(ptr);
                // std.debug.print("{any}\n", .{ips[0 .. fields.len * 2]});
                const slice = try pool.addSlice(@ptrCast(ips[0 .. fields.len * 2]));
                return .{ .tag = .object_ty, .payload = .{ .extra = slice } };
            },
            .any => .{ .tag = .any_ty, .payload = .{ .placeholder = {} } },
        };
    }

    // converts an InternPool.Item back into a Type. It is invalid
    // to call this function on an Item that is not a Type.
    pub fn deserialize(item: Item, pool: *const InternPool) Type {
        return switch (item.tag) {
            .nonetype_ty => .{ .nonetype = {} },
            .int_ty => .{ .int = {} },
            .float_ty => .{ .float = {} },
            .bool_ty => .{ .bool = {} },
            .str_ty => .{ .str = {} },
            .list_ty => .{ .list = item.payload.ip },
            .union_ty => {
                const slice = pool.extraData(item.payload.extra, Item.ExtraSlice);
                return .{ .@"union" = @ptrCast(pool.extraSlice(slice)) };
            },
            .object_ty => {
                const slice = pool.extraData(item.payload.extra, Item.ExtraSlice);
                const ips: []const InternPool.Index = @ptrCast(pool.extraSlice(slice));
                const ptr = asBytes(ips.ptr);
                const fields: [*]const Attribute = @ptrCast(ptr);
                // std.debug.print("{any}\n", .{fields[0 .. ips.len / 2]});
                return .{ .object = @ptrCast(fields[0 .. ips.len / 2]) };
            },
            .any_ty => .{ .any = {} },
            else => unreachable,
        };
    }

    pub fn hash64(ty: Type) u64 {
        const Hash = std.hash.Wyhash;
        const seed = @intFromEnum(ty);
        var hasher = Hash.init(seed);
        switch (ty) {
            .nonetype,
            .int,
            .float,
            .bool,
            .str,
            .any,
            => {},
            .list => |ip| hasher.update(asBytes(&ip)),
            inline .@"union", .object => |items| {
                for (items) |*item| hasher.update(asBytes(item));
            },
        }

        return hasher.final();
    }

    pub fn eql(a: Type, b: Type) bool {
        const a_tag = @as(Tag, a);
        const b_tag = @as(Tag, b);
        if (a_tag != b_tag) return false;

        return switch (a_tag) {
            .@"union" => std.mem.eql(InternPool.Index, a.@"union", b.@"union"),
            .object => {
                if (a.object.len == 0) return b.object.len == 0;
                return std.mem.eql(u8, asBytes(a.object), asBytes(b.object));
            },
            inline else => std.meta.eql(a, b),
        };
    }

    fn unionVariants(variants: *std.ArrayList(InternPool.Index), new: InternPool.Index) !void {
        for (variants.items) |variant| if (variant == new) return;
        try variants.append(new);
    }

    fn cmpIndex(ctx: void, a: InternPool.Index, b: InternPool.Index) bool {
        _ = ctx;
        return @intFromEnum(a) < @intFromEnum(b);
    }

    // this is not a trivially cheap function because it allocates and potentially
    // performs O(n^2) merge and sort, but we expect the input types to be "nice"
    // in many cases. if it ends up being hot, we can optimize it further
    pub fn unionTypes(arena: Allocator, pool: *InternPool, a: Type, b: Type) !Type {
        // short circuit if a == b to avoid allocating and sorting
        if (a.eql(b)) return a;

        // short circuit if a and b are non-union types for a "cheap" alloc/sort
        if (a != .@"union" and b != .@"union") {
            const a_index = try pool.put(.{ .ty = a });
            const b_index = try pool.put(.{ .ty = b });
            const variants = try arena.alloc(InternPool.Index, 2);
            if (cmpIndex({}, a_index, b_index)) {
                variants[0] = a_index;
                variants[1] = b_index;
            } else {
                variants[0] = b_index;
                variants[1] = a_index;
            }
            return .{ .@"union" = variants };
        }

        // if we get here, one or both of the types are unions, which need
        // to be unpacked (flattened), merged, sorted, and re-packed
        var variants = std.ArrayList(InternPool.Index).init(arena);
        switch (a) {
            .@"union" => |types| for (types) |variant| {
                try unionVariants(&variants, variant);
            },
            else => {
                const variant = try pool.put(.{ .ty = a });
                try unionVariants(&variants, variant);
            },
        }

        switch (b) {
            .@"union" => |types| for (types) |variant| {
                try unionVariants(&variants, variant);
            },
            else => {
                const variant = try pool.put(.{ .ty = b });
                try unionVariants(&variants, variant);
            },
        }

        // to prevent aliasing between union[a, b] and union[b, a], we must
        // establish a canonical ordering of the variants by sorting
        // note: this is an unsafe operation to perform on an arraylist
        // but its ok here because we're done inserting
        std.mem.sort(InternPool.Index, variants.items, {}, cmpIndex);

        if (variants.items.len == 1) {
            // clean up unions with only a single variant, which messes up
            // type analysis
            return pool.get(variants.items[0]).ty;
        } else {
            return .{ .@"union" = variants.items };
        }
    }

    pub fn objectSlot(object: Type, name: InternPool.Index) ?u32 {
        for (object.object, 0..) |*attr, i| {
            if (attr.name == name) return @intCast(i);
        }

        return null;
    }

    pub fn objectInsert(current: Type, arena: Allocator, new: Attribute) !Type {
        const object = current.object;
        var attributes = try std.ArrayList(Attribute).initCapacity(arena, object.len + 1);
        attributes.appendSliceAssumeCapacity(object);

        for (attributes.items) |*attr| {
            if (attr.name == new.name) {
                attr.ty = new.ty;
                // no need to insert a new attribute, so finalize the type here
                return .{ .object = attributes.items };
            }
        }
        attributes.appendAssumeCapacity(new);
        return .{ .object = attributes.items };
    }

    pub const common = struct {
        pub const nonetype: Type = .{ .nonetype = {} };
        pub const int: Type = .{ .int = {} };
        pub const float: Type = .{ .float = {} };
        pub const @"bool": Type = .{ .bool = {} };
        pub const str: Type = .{ .str = {} };
        pub const list_int: Type = .{ .list = .int };
        pub const list_any: Type = .{ .list = .any };
        pub const any: Type = .{ .any = {} };
        pub const object_empty: Type = .{ .object = &.{} };
    };
};

test "union types" {
    const unionTypes = Type.unionTypes;

    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();
    const failing = std.testing.failing_allocator;

    const int_type: Type = .{ .int = {} };
    const float_type: Type = .{ .float = {} };
    const bool_type: Type = .{ .bool = {} };
    try std.testing.expectEqual(int_type, try unionTypes(failing, &pool, int_type, int_type));
    try std.testing.expectEqual(float_type, try unionTypes(failing, &pool, float_type, float_type));
    try std.testing.expectEqual(bool_type, try unionTypes(failing, &pool, bool_type, bool_type));

    // note: these union literals must be ordered per the definitions in the
    // InternPool.Index enum. This is not recommended usage for unions, prefer
    // unionTypes() over explicit union types.
    const int_float: Type = .{ .@"union" = &.{ .int, .float } };
    // union of two primitives
    try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, int_type, float_type)));
    try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, float_type, int_type)));
    // equality of unions
    try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, int_float, int_float)));

    const int_float_bool: Type = .{ .@"union" = &.{ .int, .float, .bool } };
    // union | primitive
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, bool_type)));
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, bool_type, int_float)));
    // fully overlapping
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, int_float_bool)));
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float_bool, int_float)));

    const float_bool: Type = .{ .@"union" = &.{ .float, .bool } };
    // partially overlapping
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, float_bool)));
    try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, float_bool, int_float)));
}
