const std = @import("std");
const builtin = @import("builtin");
const Type = @import("type.zig").Type;
const TypedValue = @import("value.zig").TypedValue;
const Ir = @import("ir/Ir.zig");

const InternPool = @This();
const Allocator = std.mem.Allocator;

gpa: Allocator,
// used to map keys to indices in `items` using standard
// hashing and probing
map: std.AutoArrayHashMapUnmanaged(void, void),
// the fundamental backing stores of the pool - stores a
// enum tag (representing the kind of object) and a single u32
items: std.MultiArrayList(Item),
// extra u32 store for objects
extra: std.ArrayListUnmanaged(u32),
// extra u64 store for objects, primarily used for 64 bit constants
wide: std.ArrayListUnmanaged(u64),
// a list of all function (and toplevel) Irs processed by the frontend
irs: std.SegmentedList(Ir, 1),
// backing store for string interning
bytes: std.ArrayListUnmanaged(u8),
// probing table for interning strings in the `bytes` backing store
string_table: std.HashMapUnmanaged(StringIndex, void, IndexContext, std.hash_map.default_max_load_percentage),

pub const ExtraIndex = enum(u32) { _ };
pub const WideIndex = enum(u32) { _ };
pub const StringIndex = enum(u32) { _ };

pub const Key = union(enum) {
    ty: Type,
    tv: TypedValue,
    str: []const u8,

    const Tag = std.meta.Tag(Key);

    const Adapter = struct {
        pool: *InternPool,

        pub fn eql(adapter: Adapter, a: Key, b_void: void, b_map_index: usize) bool {
            _ = b_void;
            const b = adapter.pool.get(@enumFromInt(b_map_index));
            return b.eql(a, adapter.pool);
        }

        pub fn hash(adapter: Adapter, key: Key) u32 {
            _ = adapter;
            return key.hash32();
        }
    };

    fn hash32(key: Key) u32 {
        return @truncate(key.hash64());
    }

    fn hash64(key: Key) u64 {
        const Hash = std.hash.Wyhash;
        // const asBytes = std.mem.asBytes;
        const seed = @intFromEnum(key);

        return switch (key) {
            .ty => |ty| ty.hash64(),
            .tv => |tv| tv.hash64(),
            .str => |str| Hash.hash(seed, str),
        };
    }

    fn eql(a: Key, b: Key, pool: *const InternPool) bool {
        _ = pool;

        const a_tag = @as(Key.Tag, a);
        const b_tag = @as(Key.Tag, b);
        if (a_tag != b_tag) return false;

        switch (a_tag) {
            inline .ty, .tv => |tag| {
                const a_data = @field(a, @tagName(tag));
                const b_data = @field(b, @tagName(tag));
                return std.meta.eql(a_data, b_data);
            },
            .str => return std.mem.eql(u8, a.str, b.str),
        }
    }
};

pub const Item = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        nonetype_ty,
        int_ty,
        float_ty,
        bool_ty,
        none_tv,
        int_tv,
        float_tv,
        bool_tv,
        str,
    };

    pub const Payload = union {
        placeholder: void,
        // bools are the only constant small enough to encode inline
        bool: bool,
        // index into item array (reference to another item)
        ip: Index,
        // index into extra data array (32 bit store)
        extra: ExtraIndex,
        // index into wide data array (64 bit store)
        wide: WideIndex,
        // index into string table
        str: StringIndex,

        comptime {
            if (builtin.mode != .Debug) {
                std.debug.assert(@sizeOf(Payload) <= 4);
            }
        }
    };
};

pub const Index = enum(u32) {
    // common items are created at init time and given
    // reserved indices
    nonetype,
    int,
    float,
    bool,

    none,
    true,
    false,
    izero,
    ione,
    fzero,

    _,
};

const IndexContext = struct {
    bytes: *std.ArrayListUnmanaged(u8),

    pub fn eql(self: IndexContext, a: StringIndex, b: StringIndex) bool {
        _ = self;
        return a == b;
    }

    pub fn hash(self: IndexContext, index: StringIndex) u64 {
        const x: u32 = @intFromEnum(index);
        const str = std.mem.span(@as([*:0]const u8, @ptrCast(self.bytes.items.ptr)) + x);
        return std.hash_map.hashString(str);
    }
};

const SliceAdapter = struct {
    bytes: *std.ArrayListUnmanaged(u8),

    pub fn eql(self: SliceAdapter, a_str: []const u8, b: u32) bool {
        const b_str = std.mem.span(@as([*:0]const u8, @ptrCast(self.bytes.items.ptr)) + b);
        return std.mem.eql(u8, a_str, b_str);
    }

    pub fn hash(self: SliceAdapter, str: []const u8) u64 {
        _ = self;
        return std.hash_map.hashString(str);
    }
};

const static_keys = [_]Key{
    .{ .ty = Type.common.nonetype },
    .{ .ty = Type.common.int },
    .{ .ty = Type.common.float },
    .{ .ty = Type.common.bool },
    .{ .tv = TypedValue.common.none },
    .{ .tv = TypedValue.common.true },
    .{ .tv = TypedValue.common.false },
    .{ .tv = TypedValue.common.izero },
    .{ .tv = TypedValue.common.ione },
    .{ .tv = TypedValue.common.fzero },
};

pub fn init(gpa: Allocator) !InternPool {
    var pool: InternPool = .{
        .gpa = gpa,
        .map = .{},
        .items = .{},
        .extra = .{},
        .wide = .{},
        .irs = .{},
        .bytes = .{},
        .string_table = .{},
    };

    for (static_keys) |key| _ = try pool.put(key);
    return pool;
}

pub fn deinit(pool: *InternPool) void {
    const gpa = pool.gpa;

    pool.map.deinit(gpa);
    pool.items.deinit(gpa);
    pool.extra.deinit(gpa);
    pool.wide.deinit(gpa);
    pool.bytes.deinit(gpa);
    pool.string_table.deinit(gpa);
}

pub fn put(pool: *InternPool, key: Key) !Index {
    const adapter: Key.Adapter = .{ .pool = pool };
    const gop = try pool.map.getOrPutAdapted(pool.gpa, key, adapter);
    if (gop.found_existing) return @enumFromInt(gop.index);

    try pool.items.append(pool.gpa, switch (key) {
        .ty => |ty| try ty.serialize(pool),
        .tv => |tv| try tv.serialize(pool),
        .str => |str| try pool.putString(str),
    });
    return @enumFromInt(pool.items.len - 1);
}

pub fn get(pool: *InternPool, _index: Index) Key {
    const index: u64 = @intFromEnum(_index);
    std.debug.assert(index < pool.items.len);

    const item = pool.items.get(index);
    return switch (item.tag) {
        .nonetype_ty,
        .int_ty,
        .float_ty,
        .bool_ty,
        => .{ .ty = Type.deserialize(item, pool) },
        .none_tv,
        .int_tv,
        .float_tv,
        .bool_tv,
        => .{ .tv = TypedValue.deserialize(item, pool) },
        .str => .{ .str = pool.getString(item) },
    };
}

fn putString(pool: *InternPool, str: []const u8) !Item {
    const index: StringIndex = @enumFromInt(@as(u32, @intCast(pool.bytes.items.len)));

    try pool.bytes.ensureUnusedCapacity(pool.gpa, str.len + 1);
    pool.bytes.appendSliceAssumeCapacity(str);
    pool.bytes.appendAssumeCapacity('\x00');

    const index_context: IndexContext = .{ .bytes = &pool.bytes };
    try pool.string_table.putContext(pool.gpa, index, {}, index_context);

    return .{ .tag = .str, .payload = .{ .str = index } };
}

fn getString(pool: *InternPool, item: Item) []const u8 {
    const offset: u32 = @intFromEnum(item.payload.str);
    return std.mem.span(@as([*:0]const u8, @ptrCast(pool.bytes.items.ptr)) + offset);
}

fn testRoundtrip(pool: *InternPool, key: Key) !void {
    // put the key into the pool and record the index
    const index = try pool.put(key);
    // put it in two more times and make sure we get the same index
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expectEqual(index, try pool.put(key));

    // index -> key -> index roundtrip
    var key_rt = pool.get(index);
    try std.testing.expect(std.meta.eql(key, key_rt));
    try std.testing.expectEqual(index, try pool.put(key_rt));

    // key -> index -> key roundtrip
    const index_rt = try pool.put(key);
    try std.testing.expectEqual(index, index_rt);
    key_rt = pool.get(index_rt);
    try std.testing.expect(std.meta.eql(key, key_rt));
}

fn testIndex(pool: *InternPool, key: Key, index: Index) !void {
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expect(std.meta.eql(key, pool.get(index)));
}

test "basic type intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .ty = .{ .nonetype = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .int = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .float = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .bool = {} } });
}

test "none intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .nonetype, .val = .{ .none = {} } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.none });
    try testIndex(&pool, .{ .tv = .{ .ty = .nonetype, .val = .{ .none = {} } } }, .none);
    try testIndex(&pool, .{ .tv = TypedValue.common.none }, .none);
}

test "bool intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = true } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.true });
    try testIndex(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = true } } }, .true);
    try testIndex(&pool, .{ .tv = TypedValue.common.true }, .true);

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = false } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.false });
    try testIndex(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = false } } }, .false);
    try testIndex(&pool, .{ .tv = TypedValue.common.false }, .false);
}

test "int intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 0 } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.izero });
    try testIndex(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 0 } } }, .izero);
    try testIndex(&pool, .{ .tv = TypedValue.common.izero }, .izero);

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 1 } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.ione });
    try testIndex(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 1 } } }, .ione);
    try testIndex(&pool, .{ .tv = TypedValue.common.ione }, .ione);

    var rng = std.Random.Xoshiro256.init(0);
    var dedup = std.ArrayList(struct { val: u64, index: Index }).init(std.testing.allocator);
    defer dedup.deinit();

    outer: for (0..100) |_| {
        const val = rng.next();
        for (dedup.items) |item| if (item.val == val) continue :outer;

        try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = val } } });
        const index = try pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = val } } });
        for (dedup.items) |item| try std.testing.expect(index != item.index);
    }
}

test "string intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try std.testing.expectEqual(pool.bytes.items.len, 0);
    try std.testing.expectEqual(pool.string_table.count(), 0);

    // add three distinct strings
    const apple = try pool.put(.{ .str = "apple" });
    const banana = try pool.put(.{ .str = "banana" });
    const cherry = try pool.put(.{ .str = "cherry" });
    try std.testing.expectEqual(pool.bytes.items.len, 20);
    try std.testing.expectEqual(pool.string_table.count(), 3);
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));

    // adding the same string again should return the original id
    try std.testing.expectEqual(apple, try pool.put(.{ .str = "apple" }));
    try std.testing.expectEqual(pool.bytes.items.len, 20);
    try std.testing.expectEqual(pool.string_table.count(), 3);

    // partly overlapping string is a unique string
    const apfel = try pool.put(.{ .str = "apfel" });
    try std.testing.expectEqual(pool.bytes.items.len, 26);
    try std.testing.expectEqual(pool.string_table.count(), 4);
    try std.testing.expect(std.mem.eql(u8, "apfel", pool.get(apfel).str));
    try std.testing.expect(apple != apfel);
    try std.testing.expect(!std.mem.eql(u8, "apple", pool.get(apfel).str));

    // existing strings should not be modified
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));
}
