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
    @"union": []const InternPool.Index,
    any: void,

    pub const Tag = std.meta.Tag(Type);

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
            .@"union" => |types| {
                const slice = try pool.addSlice(@ptrCast(types));
                return .{ .tag = .union_ty, .payload = .{ .extra = slice } };
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
            .union_ty => {
                const slice = pool.extraData(item.payload.extra, Item.ExtraSlice);
                return .{ .@"union" = @ptrCast(pool.extraSlice(slice)) };
            },
            .any_ty => .{ .any = {} },
            else => unreachable,
        };
    }

    pub fn hash64(ty: Type) u64 {
        const Hash = std.hash.Wyhash;
        // const asBytes = std.mem.asBytes;
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
            .@"union" => |types| hasher.update(asBytes(types)),
        }

        return hasher.final();
    }

    pub const common = struct {
        pub const nonetype: Type = .{ .nonetype = {} };
        pub const int: Type = .{ .int = {} };
        pub const float: Type = .{ .float = {} };
        pub const @"bool": Type = .{ .bool = {} };
        pub const str: Type = .{ .str = {} };
        pub const any: Type = .{ .any = {} };
    };
};
