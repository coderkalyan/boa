const std = @import("std");
const builtin = @import("builtin");
const Type = @import("type.zig").Type;
const TypedValue = @import("value.zig").TypedValue;
const Ir = @import("ir/Ir.zig");
const Ast = @import("Ast.zig");
const Bytecode = @import("bc/Bytecode.zig");

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
// extra i64 store for objects, primarily used for 64 bit constants
wide: std.ArrayListUnmanaged(i64),
// a list of all statically allocated functions
functions: std.SegmentedList(Function, 1),
// a list of all generated function (and toplevel) Irs
irs: std.SegmentedList(Ir, 1),
// a list of all generated bytecodes
bytecodes: std.SegmentedList(Bytecode, 1),
// backing store for string interning
bytes: std.ArrayListUnmanaged(u8),
// probing table for interning strings in the `bytes` backing store
string_table: std.HashMapUnmanaged(StringIndex, void, IndexContext, std.hash_map.default_max_load_percentage),

pub const ExtraIndex = enum(u32) { _ };
pub const WideIndex = enum(u32) { _ };
pub const StringIndex = enum(u32) { _ };
pub const IrIndex = enum(u32) { toplevel, _ };
pub const BytecodeIndex = enum(u32) { toplevel, _ };
pub const FunctionIndex = enum(u32) { _ };

pub const Function = struct {
    state: State,
    tree: *const Ast,
    node: Ast.Node.Index,
    return_type: InternPool.Index,
    // global context type as the function sees it
    global: InternPool.Index,
    // invalid if state == .lazy
    ir: IrIndex,
    // invalid if state == .lazy
    bytecode: BytecodeIndex,

    pub const State = enum(u32) {
        lazy,
        interpreted,
        optimized_lite,
        optimized_full,
    };
};

pub const Key = union(enum) {
    ty: Type,
    tv: TypedValue,
    str: []const u8,
    function: FunctionIndex,
    ir: IrIndex,
    bytecode: BytecodeIndex,

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
        const asBytes = std.mem.asBytes;
        const seed = @intFromEnum(key);

        return switch (key) {
            .ty => |ty| ty.hash64(),
            .tv => |tv| tv.hash64(),
            .str => |str| Hash.hash(seed, str),
            inline .function,
            .ir,
            .bytecode,
            => |index| Hash.hash(seed, asBytes(&index)),
        };
    }

    fn eql(a: Key, b: Key, pool: *const InternPool) bool {
        _ = pool;

        const a_tag = @as(Key.Tag, a);
        const b_tag = @as(Key.Tag, b);
        if (a_tag != b_tag) return false;

        switch (a_tag) {
            .ty => return a.ty.eql(b.ty),
            inline .tv,
            .function,
            .ir,
            .bytecode,
            => |tag| {
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
        str_ty,
        list_ty,
        union_ty,
        object_ty,
        any_ty,
        none_tv,
        int_tv,
        float_tv,
        bool_tv,
        str,
        function,
        ir,
        bytecode,
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
        // index into functions list
        function: FunctionIndex,
        // index into irs list
        ir: IrIndex,
        // index into bytecode list
        bytecode: BytecodeIndex,

        comptime {
            if (builtin.mode == .ReleaseFast) {
                std.debug.assert(@sizeOf(Payload) <= 4);
            }
        }
    };

    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };
};

pub const Index = enum(u32) {
    // common items are created at init time and given
    // reserved indices
    nonetype,
    int,
    float,
    bool,
    str,
    list_int,
    list_any,
    any,
    object_empty,

    none,
    true,
    false,
    izero,
    ione,
    fzero,

    builtin_int,
    builtin_float,
    builtin_bool,
    builtin_print,
    builtin_len,
    builtin_append,

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
    .{ .ty = Type.common.str },
    .{ .ty = Type.common.list_int },
    .{ .ty = Type.common.list_any },
    .{ .ty = Type.common.any },
    .{ .ty = Type.common.object_empty },
    .{ .tv = TypedValue.common.none },
    .{ .tv = TypedValue.common.true },
    .{ .tv = TypedValue.common.false },
    .{ .tv = TypedValue.common.izero },
    .{ .tv = TypedValue.common.ione },
    .{ .tv = TypedValue.common.fzero },
    .{ .str = "int" },
    .{ .str = "float" },
    .{ .str = "bool" },
    .{ .str = "print" },
    .{ .str = "len" },
    .{ .str = "append" },
};

pub fn init(gpa: Allocator) !InternPool {
    var pool: InternPool = .{
        .gpa = gpa,
        .map = .{},
        .items = .{},
        .extra = .{},
        .wide = .{},
        .functions = .{},
        .irs = .{},
        .bytecodes = .{},
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

fn addExtra(pool: *InternPool, extra: anytype) !ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try pool.extra.ensureUnusedCapacity(pool.gpa, fields.len);
    const len: u32 = @intCast(pool.extra.items.len);
    inline for (fields) |field| {
        switch (field.type) {
            inline else => {
                const num: u32 = @intFromEnum(@field(extra, field.name));
                pool.extra.appendAssumeCapacity(@bitCast(num));
            },
        }
    }

    return @enumFromInt(len);
}

pub fn extraData(pool: *const InternPool, index: ExtraIndex, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(pool.extra.items[base + i]),
        }
    }
    return result;
}

pub fn addSlice(pool: *InternPool, sl: []const u32) !ExtraIndex {
    const start: u32 = @intCast(pool.extra.items.len);
    try pool.extra.appendSlice(pool.gpa, sl);
    const end: u32 = @intCast(pool.extra.items.len);
    return pool.addExtra(Item.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    });
}

pub fn extraSlice(pool: *const InternPool, sl: Item.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(sl.start);
    const end: u32 = @intFromEnum(sl.end);
    return pool.extra.items[start..end];
}

pub fn put(pool: *InternPool, key: Key) !Index {
    const adapter: Key.Adapter = .{ .pool = pool };
    const gop = try pool.map.getOrPutAdapted(pool.gpa, key, adapter);
    if (gop.found_existing) return @enumFromInt(gop.index);

    try pool.items.append(pool.gpa, switch (key) {
        .ty => |ty| try ty.serialize(pool),
        .tv => |tv| try tv.serialize(pool),
        .str => |str| try pool.putString(str),
        .function => |function| .{ .tag = .function, .payload = .{ .function = function } },
        .ir => |ir| .{ .tag = .ir, .payload = .{ .ir = ir } },
        .bytecode => |bytecode| .{ .tag = .bytecode, .payload = .{ .bytecode = bytecode } },
    });
    return @enumFromInt(pool.items.len - 1);
}

pub fn get(pool: *const InternPool, _index: Index) Key {
    const index: u64 = @intFromEnum(_index);
    std.debug.assert(index < pool.items.len);

    const item = pool.items.get(index);
    return switch (item.tag) {
        .nonetype_ty,
        .int_ty,
        .float_ty,
        .bool_ty,
        .str_ty,
        .list_ty,
        .union_ty,
        .object_ty,
        .any_ty,
        => .{ .ty = Type.deserialize(item, pool) },
        .none_tv,
        .int_tv,
        .float_tv,
        .bool_tv,
        => .{ .tv = TypedValue.deserialize(item, pool) },
        .str => .{ .str = pool.getString(item) },
        .function => .{ .function = item.payload.function },
        .ir => .{ .ir = item.payload.ir },
        .bytecode => .{ .bytecode = item.payload.bytecode },
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

fn getString(pool: *const InternPool, item: Item) []const u8 {
    const offset: u32 = @intFromEnum(item.payload.str);
    return std.mem.span(@as([*:0]const u8, @ptrCast(pool.bytes.items.ptr)) + offset);
}

pub fn createFunction(pool: *InternPool, fi: Function) !FunctionIndex {
    const index: u32 = @intCast(pool.functions.count());
    try pool.functions.append(pool.gpa, fi);
    return @enumFromInt(index);
}

pub fn functionPtr(pool: *InternPool, index: FunctionIndex) *Function {
    return pool.functions.at(@intFromEnum(index));
}

pub fn createIr(pool: *InternPool, ir: Ir) !IrIndex {
    const index: u32 = @intCast(pool.irs.count());
    try pool.irs.append(pool.gpa, ir);
    return @enumFromInt(index);
}

pub fn irPtr(pool: *InternPool, index: IrIndex) *Ir {
    return pool.irs.at(@intFromEnum(index));
}

pub fn createBytecode(pool: *InternPool, bytecode: Bytecode) !BytecodeIndex {
    const index: u32 = @intCast(pool.bytecodes.count());
    try pool.bytecodes.append(pool.gpa, bytecode);
    return @enumFromInt(index);
}

pub fn bytecodePtr(pool: *InternPool, index: BytecodeIndex) *Bytecode {
    return pool.bytecodes.at(@intFromEnum(index));
}

pub fn print(pool: *const InternPool, writer: anytype, ip: Index) !void {
    const key = pool.get(ip);
    switch (key) {
        .ty => |ty| switch (ty) {
            inline else => try writer.print("{s}", .{@tagName(ty)}),
        },
        .tv => |tv| {
            try pool.print(writer, tv.ty);
            try writer.print("(", .{});
            switch (tv.val) {
                .none => try writer.print("none", .{}),
                inline .int, .float, .bool => |val| try writer.print("{}", .{val}),
            }
            try writer.print(")", .{});
        },
        .str => |str| try writer.print("\"{s}\"", .{str}),
        .function => |function| try writer.print("function{}", .{@intFromEnum(function)}),
        .ir => |ir| try writer.print("ir{}", .{@intFromEnum(ir)}),
        .bytecode => |bytecode| try writer.print("bytecode{}", .{@intFromEnum(bytecode)}),
    }
}

fn testRoundtrip(pool: *InternPool, key: Key) !void {
    // put the key into the pool and record the index
    const index = try pool.put(key);
    // put it in two more times and make sure we get the same index
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expectEqual(index, try pool.put(key));

    // index -> key -> index roundtrip
    var key_rt = pool.get(index);
    try std.testing.expect(key.eql(key_rt, pool));
    try std.testing.expectEqual(index, try pool.put(key_rt));

    // key -> index -> key roundtrip
    const index_rt = try pool.put(key);
    try std.testing.expectEqual(index, index_rt);
    key_rt = pool.get(index_rt);
    try std.testing.expect(key.eql(key_rt, pool));
}

fn testIndex(pool: *InternPool, key: Key, index: Index) !void {
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expect(key.eql(pool.get(index), pool));
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

test "union type intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .ty = .{ .@"union" = &.{.int} } });
    try testRoundtrip(&pool, .{ .ty = .{ .@"union" = &.{.float} } });
    try testRoundtrip(&pool, .{ .ty = .{ .@"union" = &.{.bool} } });
    try testRoundtrip(&pool, .{ .ty = .{ .@"union" = &.{ .int, .float } } });
    try testRoundtrip(&pool, .{ .ty = .{ .@"union" = &.{ .int, .float, .bool } } });
}

test "object type intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    const apple = try pool.put(.{ .str = "apple" });
    const banana = try pool.put(.{ .str = "banana" });
    const cherry = try pool.put(.{ .str = "cherry" });

    const object_empty: Type = .{ .object = &.{} };
    try testRoundtrip(&pool, .{ .ty = object_empty });
    try testIndex(&pool, .{ .ty = object_empty }, .object_empty);

    const int_float: Type = .{
        .object = &.{
            .{ .name = apple, .ty = .int },
            .{ .name = banana, .ty = .float },
        },
    };
    try testRoundtrip(&pool, .{ .ty = int_float });

    const int_float_bool: Type = .{
        .object = &.{
            .{ .name = apple, .ty = .int },
            .{ .name = banana, .ty = .float },
            .{ .name = cherry, .ty = .bool },
        },
    };
    try testRoundtrip(&pool, .{ .ty = int_float_bool });
    const if_index = try pool.put(.{ .ty = int_float });
    const ifb_index = try pool.put(.{ .ty = int_float_bool });
    try std.testing.expect(if_index != ifb_index);
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

        try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = @bitCast(val) } } });
        const index = try pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = @bitCast(val) } } });
        for (dedup.items) |item| try std.testing.expect(index != item.index);
    }
}

test "string intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // add three distinct strings
    const apple = try pool.put(.{ .str = "apple" });
    const banana = try pool.put(.{ .str = "banana" });
    const cherry = try pool.put(.{ .str = "cherry" });
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));

    // adding the same string again should return the original id
    try std.testing.expectEqual(apple, try pool.put(.{ .str = "apple" }));

    // partly overlapping string is a unique string
    const apfel = try pool.put(.{ .str = "apfel" });
    try std.testing.expect(std.mem.eql(u8, "apfel", pool.get(apfel).str));
    try std.testing.expect(apple != apfel);
    try std.testing.expect(!std.mem.eql(u8, "apple", pool.get(apfel).str));

    // existing strings should not be modified
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));
}
