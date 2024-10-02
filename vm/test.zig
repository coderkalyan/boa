const std = @import("std");
const Bytecode = @import("Bytecode.zig");

const Allocator = std.mem.Allocator;
const Opcode = Bytecode.Opcode;

var trap_ip: [*]const i32 = undefined;
var trap_fp: [*]i64 = undefined;
var trap_sp: [*]i64 = undefined;
var trap_ctx: *anyopaque = undefined;

extern fn interpreter_entry(ip: [*]const i32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void;
export fn interpreter_trap_inner(ip: [*]const i32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void {
    trap_ip = ip;
    trap_fp = fp;
    trap_sp = sp;
    trap_ctx = ctx;
}

fn allocStack(arena: Allocator, in_stack: anytype) ![]i64 {
    const out_stack = try arena.alloc(i64, in_stack.len);
    inline for (in_stack, 0..) |val, i| {
        out_stack[i] = switch (@TypeOf(val)) {
            comptime_int, i64 => val,
            u64 => @bitCast(val),
            comptime_float => @bitCast(@as(f64, val)),
            f64 => @bitCast(val),
            else => unreachable,
        };
    }

    return out_stack;
}

fn allocTape(arena: Allocator, in_tape: anytype) ![]i32 {
    const out_tape = try arena.alloc(i32, in_tape.len + 1);
    inline for (in_tape, 0..) |val, i| {
        out_tape[i] = switch (@TypeOf(val)) {
            comptime_int, i32 => val,
            u32 => @bitCast(val),
            Opcode => @intFromEnum(val),
            else => unreachable,
        };
    }

    out_tape[in_tape.len] = @intFromEnum(Opcode.trap);
    return out_tape;
}

fn runTest(arena: Allocator, comptime testcase: anytype) !void {
    const tape = try allocTape(arena, testcase.in_tape);
    const in_stack = try allocStack(arena, testcase.in_stack);
    const out_stack = try allocStack(arena, testcase.out_stack);

    const ip = tape.ptr;
    const fp = in_stack.ptr;
    const sp = fp + testcase.frame_size;
    interpreter_entry(ip, fp, sp, undefined);

    try std.testing.expectEqual(ip + testcase.offsets[0], trap_ip);
    try std.testing.expectEqual(fp + testcase.offsets[1], trap_fp);
    try std.testing.expectEqual(sp + testcase.offsets[2], trap_sp);
    try std.testing.expectEqualSlices(i64, out_stack, in_stack);
}

test "ld" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, .{
        .in_tape = .{ Opcode.ld, 0, 100 },
        .in_stack = .{ 0, 0, 0 },
        .frame_size = 2,
        .out_stack = .{ 100, 0, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, .{
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
        try runTest(arena, .{
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
        try runTest(arena, .{
            .in_tape = .{ Opcode.ldw, 1, lower, upper },
            .in_stack = .{ 0, 0, 0 },
            .frame_size = 2,
            .out_stack = .{ 0, val, 0 },
            .offsets = .{ 4, 0, 0 },
        });
    }
}

test "mov" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try runTest(arena, .{
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

    try runTest(arena, .{
        .in_tape = .{ Opcode.ineg, 0, 1 },
        .in_stack = .{ 0, 123, 0 },
        .frame_size = 2,
        .out_stack = .{ -123, 123, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, .{
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

    try runTest(arena, .{
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
    try runTest(arena, .{
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

    try runTest(arena, .{
        .in_tape = .{ Opcode.lnot, 0, 1 },
        .in_stack = .{ 0, 0, 0 },
        .frame_size = 2,
        .out_stack = .{ 1, 0, 0 },
        .offsets = .{ 3, 0, 0 },
    });

    try runTest(arena, .{
        .in_tape = .{ Opcode.lnot, 0, 1 },
        .in_stack = .{ 0, 1, 0 },
        .frame_size = 2,
        .out_stack = .{ 0, 1, 0 },
        .offsets = .{ 3, 0, 0 },
    });
}
