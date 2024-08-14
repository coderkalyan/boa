const std = @import("std");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const Item = InternPool.Item;

pub const Value = union(enum) {
    none: void,
    int: u64,
    float: f64,
    bool: bool,

    pub const Tag = std.meta.Tag(Value);
};

pub const TypedValue = struct {
    ty: InternPool.Index,
    val: Value,

    // converts a Value into an InternPool.Item for efficient
    // storage. Extra serialization storage is available via
    // the .extra and .wide arrays owned by the pool.
    pub fn serialize(tv: TypedValue, pool: *InternPool) !Item {
        const val = tv.val;

        return switch (val) {
            // because none, ints, floats, and bools have "known" types
            .none => .{ .tag = .none_tv, .payload = .{ .placeholder = {} } },
            // with fixed indices, there's no need to store them
            .int => |int| {
                const wide_top: u32 = @intCast(pool.wide.items.len);
                try pool.wide.append(pool.gpa, int);

                return .{ .tag = .int_tv, .payload = .{ .wide = @enumFromInt(wide_top) } };
            },
            .float => |float| {
                const wide_top: u32 = @intCast(pool.wide.items.len);
                try pool.wide.append(pool.gpa, @bitCast(float));

                return .{ .tag = .float_tv, .payload = .{ .wide = @enumFromInt(wide_top) } };
            },
            .bool => |b| .{ .tag = .bool_tv, .payload = .{ .bool = b } },
        };
    }

    // converts an InternPool.Item back into a TypedValue. It is invalid
    // to call this function on an Item that is not a TypedValue.
    pub fn deserialize(item: Item, pool: *const InternPool) TypedValue {
        const tag = item.tag;
        const payload = item.payload;

        return switch (tag) {
            .none_tv => .{ .ty = .nonetype, .val = .{ .none = {} } },
            .int_tv => {
                const int = pool.wide.items[@intFromEnum(payload.wide)];
                return .{ .ty = .int, .val = .{ .int = int } };
            },
            .float_tv => {
                const float: f64 = @bitCast(pool.wide.items[@intFromEnum(payload.wide)]);
                return .{ .ty = .float, .val = .{ .float = float } };
            },
            .bool_tv => .{ .ty = .bool, .val = .{ .bool = payload.bool } },
            else => unreachable,
        };
    }

    pub fn hash64(tv: TypedValue) u64 {
        const Hash = std.hash.Wyhash;
        const asBytes = std.mem.asBytes;

        const seed = @intFromEnum(tv.val);
        var hasher = Hash.init(seed);
        hasher.update(asBytes(&tv.ty));

        switch (tv.val) {
            .none => {},
            inline else => |val| hasher.update(asBytes(&val)),
        }

        return hasher.final();
    }

    pub const common = struct {
        pub const none: TypedValue = .{ .ty = .nonetype, .val = .{ .none = {} } };
        pub const @"true": TypedValue = .{ .ty = .bool, .val = .{ .bool = true } };
        pub const @"false": TypedValue = .{ .ty = .bool, .val = .{ .bool = false } };
        pub const izero: TypedValue = .{ .ty = .int, .val = .{ .int = 0 } };
        pub const ione: TypedValue = .{ .ty = .int, .val = .{ .int = 1 } };
        pub const fzero: TypedValue = .{ .ty = .float, .val = .{ .float = 0.0 } };
    };
};
