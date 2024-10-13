const std = @import("std");
const Bytecode = @import("Bytecode.zig");
const Assembler = @import("assembler.zig").Assembler;
// const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Opcode = Bytecode.Opcode;

pub const FunctionInfoLayout = struct {
    tree: *anyopaque,
    ir: *anyopaque,
    bytecode: *anyopaque,
    node: u32,
    frame_size: u32,
    state: u32,
};

pub const ContextStub = extern struct {
    ipool: *anyopaque,
    global: *anyopaque,
};

pub const ObjectStub = extern struct {
    shape: *anyopaque,
};

var trap_ip: [*]i32 = undefined;
var trap_fp: [*]i64 = undefined;
var trap_sp: [*]i64 = undefined;
var trap_ctx: *anyopaque = undefined;
var store_attr: u64 = undefined;
var store_val: i64 = undefined;
const global_shape = 0xcafe;
const ipool = 0xbeef;

extern fn interpreter_entry(ip: [*]i32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void;
export fn interpreter_trap_inner(ip: [*]i32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void {
    trap_ip = ip;
    trap_fp = fp;
    trap_sp = sp;
    trap_ctx = ctx;
}

export fn rt_dispatch(id: u32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void {
    _ = id;
    _ = fp;
    _ = sp;
    _ = ctx;
    // std.debug.print("dispatch! {} {*} {*}\n", .{ id, fp, sp });
}

export fn rt_compile(ctx: *anyopaque, fi: *anyopaque) callconv(.C) void {
    _ = ctx;
    _ = fi;
}

export fn rt_attr_index(object: *anyopaque, attr: u64) callconv(.C) i64 {
    _ = object;
    return @intCast(attr);
}

export fn rt_attr_insert(ctx: *anyopaque, object: *anyopaque, attr: u64) callconv(.C) i64 {
    _ = ctx;
    _ = object;
    return @intCast(attr);
}

export fn rt_attr_load(object: *anyopaque, attr: u64) callconv(.C) i64 {
    _ = object;
    return @intCast(attr * 10);
}

export fn rt_attr_store(object: *anyopaque, attr: u64, val: i64) callconv(.C) void {
    _ = object;
    store_attr = attr;
    store_val = val;
}

fn allocStack(arena: Allocator, in_stack: anytype) ![]i64 {
    const out_stack = try arena.alloc(i64, in_stack.len);
    inline for (in_stack, 0..) |val, i| {
        out_stack[i] = switch (@TypeOf(val)) {
            comptime_int, i64 => val,
            u64, usize => @bitCast(val),
            comptime_float => @bitCast(@as(f64, val)),
            f64 => @bitCast(val),
            else => unreachable,
        };
    }

    return out_stack;
}

fn coerceTape(ctx: *ContextStub, in_tape: anytype, out_tape: []i32) void {
    inline for (in_tape, 0..) |val, i| {
        out_tape[i] = switch (@TypeOf(val)) {
            comptime_int, i32 => val,
            u32 => @bitCast(val),
            Opcode => @intFromEnum(val),
            @TypeOf(.enum_literal) => switch (val) {
                .ctx => @bitCast(@as(u32, @truncate(@intFromPtr(ctx)))),
                else => unreachable,
            },
            else => unreachable,
        };
    }
}

fn allocTape(arena: Allocator, ctx: *ContextStub, in_tape: anytype) ![]i32 {
    const out_tape = try arena.alloc(i32, in_tape.len + 1);
    coerceTape(ctx, in_tape, out_tape);
    out_tape[in_tape.len] = @intFromEnum(Opcode.trap);
    return out_tape;
}

inline fn ptrOffset(ptr: anytype, offset: comptime_int) @TypeOf(ptr) {
    if (offset < 0) return ptr - @as(usize, -offset);
    return ptr + offset;
}

fn runTest(arena: Allocator, ctx: *ContextStub, comptime testcase: anytype) !void {
    const tape = try allocTape(arena, ctx, testcase.in_tape);
    const in_stack = try allocStack(arena, testcase.in_stack);
    const out_stack = try allocStack(arena, testcase.out_stack);

    // trap_ip = undefined;
    // trap_fp = undefined;
    // trap_sp = undefined;
    // trap_ctx = undefined;

    const ip = tape.ptr;
    const fp = in_stack.ptr;
    const sp = fp + testcase.frame_size;
    interpreter_entry(ip, fp, sp, ctx);

    try std.testing.expectEqual(ip + testcase.offsets[0], trap_ip);
    try std.testing.expectEqual(fp + testcase.offsets[1], trap_fp);
    try std.testing.expectEqual(ptrOffset(sp, testcase.offsets[2]), trap_sp);
    try std.testing.expectEqualSlices(i64, out_stack, in_stack);
    if (@hasField(@TypeOf(testcase), "out_tape")) {
        const out_tape = try allocTape(arena, ctx, testcase.out_tape);
        try std.testing.expectEqualSlices(i32, out_tape, tape);
    } else {
        const out_tape = try allocTape(arena, ctx, testcase.in_tape);
        try std.testing.expectEqualSlices(i32, out_tape, tape);
    }
}

test "ld" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.ld, 0, 100 },
        .in_stack = .{ 0, 0, 0 },
        .frame_size = 2,
        .out_stack = .{ 100, 0, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.ld, 0, -1 },
        .in_stack = .{ 0, 0, 0 },
        .frame_size = 2,
        .out_stack = .{ -1, 0, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "ldw" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    {
        const val = std.math.maxInt(i64) - 1;
        const lower: u32 = @truncate(val);
        const upper: u32 = @truncate(val >> 32);
        try runTest(arena, undefined, .{
            .in_tape = .{ Opcode.ldw, 1, lower, upper },
            .in_stack = .{ 0, 0, 0 },
            .frame_size = 2,
            .out_stack = .{ 0, val, 0 },
            .offsets = .{ 4, 0, 0 },
        });
    }

    {
        const val: u64 = @bitCast(@as(i64, std.math.minInt(i64) + 1));
        const lower: u32 = @truncate(val);
        const upper: u32 = @truncate(val >> 32);
        try runTest(arena, undefined, .{
            .in_tape = .{ Opcode.ldw, 1, lower, upper },
            .in_stack = .{ 0, 0, 0 },
            .frame_size = 2,
            .out_stack = .{ 0, val, 0 },
            .offsets = .{ 4, 0, 0 },
        });
    }
}

test "ldg_init" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var global: ObjectStub = .{ .shape = @ptrFromInt(global_shape) };
    var context: ContextStub = .{
        .ipool = @ptrFromInt(ipool),
        .global = &global,
    };

    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.ldg_init, 0, 2, 0, 0 },
        .in_stack = .{0},
        .frame_size = 1,
        .out_tape = .{ Opcode.ldg_fast, 0, 2, global_shape, 2 },
        .out_stack = .{20},
        .offsets = .{ 5, 0, 0 },
    });
}

test "ldg_fast" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var global: ObjectStub = .{ .shape = @ptrFromInt(global_shape) };
    var context: ContextStub = .{
        .ipool = @ptrFromInt(ipool),
        .global = &global,
    };

    // cache hit - ip = 2, already cached as index 2 in global
    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.ldg_fast, 0, 2, global_shape, 2 },
        .in_stack = .{0},
        .frame_size = 1,
        .out_stack = .{20},
        .offsets = .{ 5, 0, 0 },
    });

    // cache miss - ip = 2, cache has some other shape
    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.ldg_fast, 0, 2, global_shape + 1, 2 },
        .in_stack = .{0},
        .frame_size = 1,
        .out_tape = .{ Opcode.ldg_fast, 0, 2, global_shape, 2 },
        .out_stack = .{20},
        .offsets = .{ 5, 0, 0 },
    });
}

test "stg_init" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var global: ObjectStub = .{ .shape = @ptrFromInt(global_shape) };
    var context: ContextStub = .{
        .ipool = @ptrFromInt(ipool),
        .global = &global,
    };

    store_attr = 0;
    store_val = 0;
    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.stg_init, 0, 2, 0, 0 },
        .in_stack = .{30},
        .frame_size = 1,
        .out_tape = .{ Opcode.stg_fast, 0, 2, global_shape, 2 },
        .out_stack = .{30},
        .offsets = .{ 5, 0, 0 },
    });
    try std.testing.expectEqual(2, store_attr);
    try std.testing.expectEqual(30, store_val);
}

test "stg_fast" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var global: ObjectStub = .{ .shape = @ptrFromInt(global_shape) };
    var context: ContextStub = .{
        .ipool = @ptrFromInt(ipool),
        .global = &global,
    };

    store_attr = 0;
    store_val = 0;
    // cache hit - ip = 2, already cached as index 2 in global
    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.stg_fast, 0, 2, global_shape, 2 },
        .in_stack = .{30},
        .frame_size = 1,
        .out_stack = .{30},
        .offsets = .{ 5, 0, 0 },
    });
    try std.testing.expectEqual(2, store_attr);
    try std.testing.expectEqual(30, store_val);

    store_attr = 0;
    store_val = 0;
    // cache miss - ip = 2, cache has some other shape
    try runTest(arena, &context, .{
        .in_tape = .{ Opcode.stg_fast, 0, 2, global_shape + 1, 2 },
        .in_stack = .{30},
        .frame_size = 1,
        .out_tape = .{ Opcode.stg_fast, 0, 2, global_shape, 2 },
        .out_stack = .{30},
        .offsets = .{ 5, 0, 0 },
    });
    try std.testing.expectEqual(2, store_attr);
    try std.testing.expectEqual(30, store_val);
}

test "mov" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.mov, 0, 1 },
        .in_stack = .{ 0, 123, 0 },
        .frame_size = 2,
        .out_stack = .{ 123, 123, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "ineg" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.ineg, 0, 1 },
        .in_stack = .{ 0, 123, 0 },
        .frame_size = 2,
        .out_stack = .{ -123, 123, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.ineg, 0, 1 },
        .in_stack = .{ 0, -std.math.maxInt(i64), 0 },
        .frame_size = 2,
        .out_stack = .{ std.math.maxInt(i64), -std.math.maxInt(i64), 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "fneg" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.fneg, 0, 1 },
        .in_stack = .{ 0, 123.0, 0 },
        .frame_size = 2,
        .out_stack = .{ -123.0, 123.0, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "binv" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const val: u64 = 0x00001111aaaaffff;
    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.binv, 0, 1 },
        .in_stack = .{ 0, val, 0 },
        .frame_size = 2,
        .out_stack = .{ ~val, val, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "lnot" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.lnot, 0, 1 },
        .in_stack = .{ 0, 0, 0 },
        .frame_size = 2,
        .out_stack = .{ 1, 0, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.lnot, 0, 1 },
        .in_stack = .{ 0, 1, 0 },
        .frame_size = 2,
        .out_stack = .{ 0, 1, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}

test "iadd" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.iadd, 0, 0, 1 },
        .in_stack = .{ 100, 200 },
        .frame_size = 2,
        .out_stack = .{ 300, 200 },
        .offsets = .{ 4, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.iadd, 0, 0, 1 },
        .in_stack = .{ std.math.maxInt(i64), -std.math.maxInt(i64) },
        .frame_size = 2,
        .out_stack = .{ 0, -std.math.maxInt(i64) },
        .offsets = .{ 4, 0, 0 },
    });
}

test "isub" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.isub, 0, 0, 1 },
        .in_stack = .{ 100, 200 },
        .frame_size = 2,
        .out_stack = .{ -100, 200 },
        .offsets = .{ 4, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.isub, 0, 0, 1 },
        .in_stack = .{ 0, std.math.maxInt(i64) },
        .frame_size = 2,
        .out_stack = .{ -std.math.maxInt(i64), std.math.maxInt(i64) },
        .offsets = .{ 4, 0, 0 },
    });
}

test "imul" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.imul, 0, 0, 1 },
        .in_stack = .{ 123, -2 },
        .frame_size = 2,
        .out_stack = .{ -246, -2 },
        .offsets = .{ 4, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.imul, 0, 0, 1 },
        .in_stack = .{ 0, 0 },
        .frame_size = 2,
        .out_stack = .{ 0, 0 },
        .offsets = .{ 4, 0, 0 },
    });
}

test "idiv" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.idiv, 0, 0, 1 },
        .in_stack = .{ 246, -2 },
        .frame_size = 2,
        .out_stack = .{ -123, -2 },
        .offsets = .{ 4, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.idiv, 0, 0, 1 },
        .in_stack = .{ 7, 3 },
        .frame_size = 2,
        .out_stack = .{ 2, 3 },
        .offsets = .{ 4, 0, 0 },
    });
}

test "imod" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.imod, 0, 0, 1 },
        .in_stack = .{ 246, -2 },
        .frame_size = 2,
        .out_stack = .{ 0, -2 },
        .offsets = .{ 4, 0, 0 },
    });

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.imod, 0, 0, 1 },
        .in_stack = .{ 7, 3 },
        .frame_size = 2,
        .out_stack = .{ 1, 3 },
        .offsets = .{ 4, 0, 0 },
    });
}

test "push_one" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.push_one, 0 },
        .in_stack = .{ 100, 0, 0 },
        .frame_size = 1,
        .out_stack = .{ 100, 100, 0 },
        .offsets = .{ 2, 0, 1 },
    });
}

test "push_multi" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.push_multi, 4, 1, 2, 0, 3 },
        .in_stack = .{ 100, 200, 300, 400, 0, 0, 0, 0, 0 },
        .frame_size = 4,
        .out_stack = .{ 100, 200, 300, 400, 200, 300, 100, 400, 0 },
        .offsets = .{ 6, 0, 4 },
    });
}

test "pop_one" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{Opcode.pop_one},
        .in_stack = .{ 100, 0, 0 },
        .frame_size = 1,
        .out_stack = .{ 100, 0, 0 },
        .offsets = .{ 1, 0, -1 },
    });
}

test "pop_multi" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, undefined, .{
        .in_tape = .{ Opcode.pop_multi, 3 },
        .in_stack = .{ 100, 100, 200, 300 },
        .frame_size = 1,
        .out_stack = .{ 100, 100, 200, 300 },
        .offsets = .{ 2, 0, -3 },
    });
}

test "call_init" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{Opcode.trap});
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 2,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{ Opcode.call_init, 0, 0, Opcode.trap });
    const in_stack = .{ @intFromPtr(&function_info), 0, 0, 0, 200, 300 };
    const outer_frame_size = 1;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(inner_tape.ptr, trap_ip);
    try std.testing.expectEqual(fp + 5, trap_fp);
    try std.testing.expectEqual(fp + 7, trap_sp);
    try std.testing.expectEqual(@intFromEnum(Opcode.call_fast), outer_tape[0]);
}

test "call_fast" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{Opcode.trap});
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 2,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{ Opcode.call_fast, 0, 0, 0, Opcode.trap });
    const in_stack = .{ @intFromPtr(&function_info), 0, 0, 0, 100, 200, 300 };
    const outer_frame_size = 1;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(inner_tape.ptr, trap_ip);
    try std.testing.expectEqual(fp + 5, trap_fp);
    try std.testing.expectEqual(fp + 7, trap_sp);
    try std.testing.expectEqual(0, stack[4]);
}

test "ret" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{ Opcode.ret, 1 });
    const outer_tape = try allocTape(arena, undefined, .{ Opcode.call_fast, 0, 0, 0, Opcode.trap });
    const in_stack = .{ 0, 0, 0, 0, 0, 200, 300 };
    const outer_frame_size = 1;

    const stack = try allocStack(arena, in_stack);
    stack[1] = @bitCast(@intFromPtr(outer_tape.ptr + 4));
    stack[2] = @bitCast(@intFromPtr(stack.ptr));
    stack[3] = @bitCast(@intFromPtr(stack.ptr + outer_frame_size));

    const ip = inner_tape.ptr;
    const fp = stack.ptr + 5;
    const sp = stack.ptr + 7;
    interpreter_entry(ip, fp, sp, undefined);

    // TODO: this should actually save the next ip
    try std.testing.expectEqual(outer_tape.ptr + 4, trap_ip);
    try std.testing.expectEqual(stack.ptr, trap_fp);
    try std.testing.expectEqual(stack.ptr + outer_frame_size, trap_sp);
    try std.testing.expectEqual(300, stack[0]);
}

test "call and return no args" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{ Opcode.ret, 0 });
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 1,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{ Opcode.call_init, 0, 0, 0, Opcode.trap });
    const in_stack = .{ @intFromPtr(&function_info), 0, 0, 0, 0, 200 };
    const outer_frame_size = 1;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(ip + 4, trap_ip);
    try std.testing.expectEqual(fp, trap_fp);
    try std.testing.expectEqual(sp, trap_sp);
    try std.testing.expectEqual(200, stack[0]);
}

test "call and return one arg" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{ Opcode.ret, -5 - 0 });
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 1,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{
        Opcode.push_one,
        0, // arg: x0
        Opcode.call_init,
        1, // target: x1
        0, // return: x0
        0, // inline cache
        Opcode.pop_one,
        Opcode.trap,
    });
    const in_stack = .{ 300, @intFromPtr(&function_info), 0, 0, 0, 0, 200 };
    const outer_frame_size = 2;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(ip + 7, trap_ip);
    try std.testing.expectEqual(fp, trap_fp);
    try std.testing.expectEqual(sp, trap_sp);
    try std.testing.expectEqual(300, stack[0]);
}

test "call and return two args" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const inner_tape = try allocTape(arena, undefined, .{
        Opcode.iadd,
        0, // x0
        -5 - 0, // arg0
        -5 - 1, // arg1
        Opcode.ret,
        0, // x0
    });
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 1,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{
        Opcode.push_multi,
        2,
        0, // arg: x0
        1, // arg: x1
        Opcode.call_init,
        2, // target: x2
        0, // return: x0
        0, // inline cache
        Opcode.pop_multi,
        2,
        Opcode.trap,
    });
    const in_stack = .{ 300, 200, @intFromPtr(&function_info), 0, 0, 0, 0, 0 };
    const outer_frame_size = 3;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(ip + 10, trap_ip);
    try std.testing.expectEqual(fp, trap_fp);
    try std.testing.expectEqual(sp, trap_sp);
    try std.testing.expectEqual(500, stack[0]);
}

// test "callrt" {
//     var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena_allocator.deinit();
//     const arena = arena_allocator.allocator();
//
//     try runTest(arena, undefined, .{
//         .in_tape = .{ Opcode.callrt, 0, 0 },
//         .in_stack = .{ 100, 100, 200, 300 },
//         .frame_size = 1,
//         .out_stack = .{ 100, 100, 200, 300 },
//         .offsets = .{ 3, 0, 0 },
//     });
// }

pub fn main() !void {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var global: ObjectStub = .{ .shape = @ptrFromInt(global_shape) };
    var context: ContextStub = .{
        .ipool = @ptrFromInt(ipool),
        .global = &global,
    };

    const inner_tape = try allocTape(arena, undefined, .{ Opcode.ret, 0 });
    var function_info: FunctionInfoLayout = .{
        .tree = undefined,
        .ir = undefined,
        .bytecode = inner_tape.ptr,
        .node = undefined,
        .frame_size = 1,
        .state = 1,
    };

    const outer_tape = try allocTape(arena, undefined, .{ Opcode.call_init, 0, 0, Opcode.trap });
    const in_stack = .{ @intFromPtr(&function_info), 0, 0, 0, 0, 200 };
    const outer_frame_size = 1;

    const stack = try allocStack(arena, in_stack);

    const ip = outer_tape.ptr;
    const fp = stack.ptr;
    const sp = fp + outer_frame_size;
    interpreter_entry(ip, fp, sp, &context);

    // const tape = try allocTape(arena, &context, .{ Opcode.ldg_fast, 0, 2, .ctx, 2 });
    // const in_stack = try allocStack(arena, undefined, .{0});
    //
    // const ip = tape.ptr;
    // const fp = in_stack.ptr;
    // const sp = fp + 1;
    // const ctx = &context;

    // for (0..100_000) |_| {
    // interpreter_entry(ip, fp, sp, ctx);
    // }
}
