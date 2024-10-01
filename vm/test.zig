const std = @import("std");
const Bytecode = @import("Bytecode.zig");

const testing = std.testing;
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

fn runTape(input_tape: []const i32, ip_offset: usize, fp_offset: usize, sp_offset: usize) ![]const i64 {
    const stack = try testing.allocator.alloc(i64, 16);

    const tape = try testing.allocator.alloc(i32, input_tape.len + 1);
    defer testing.allocator.free(tape);
    @memcpy(tape[0 .. tape.len - 1], input_tape);
    tape[tape.len - 1] = @intFromEnum(Opcode.trap);

    const ip = tape.ptr;
    const fp = stack.ptr;
    const sp = fp;
    const ctx: *anyopaque = undefined;
    interpreter_entry(ip, fp, sp, ctx);

    try testing.expectEqual(ip + ip_offset, trap_ip);
    try testing.expectEqual(fp + fp_offset, trap_fp);
    try testing.expectEqual(sp + sp_offset, trap_sp);
    return stack;
}

test "ld" {
    {
        const tape = &.{ @intFromEnum(Opcode.ld), 0, 100 };
        const stack = try runTape(tape, 3, 0, 0);
        defer testing.allocator.free(stack);
        try testing.expectEqual(100, stack[0]);
    }

    {
        const tape = &.{ @intFromEnum(Opcode.ld), 0, -1 };
        const stack = try runTape(tape, 3, 0, 0);
        defer testing.allocator.free(stack);
        try testing.expectEqual(-1, stack[0]);
    }
}

test "ldw" {
    {
        const val: u64 = @bitCast(@as(i64, std.math.maxInt(i64)) - 1);
        const lower: u32 = @truncate(val);
        const upper: u32 = @truncate(val >> 32);
        const tape = &.{
            @intFromEnum(Opcode.ldw),
            2,
            @as(i32, @bitCast(lower)),
            @as(i32, @bitCast(upper)),
        };

        const stack = try runTape(tape, 4, 0, 0);
        defer testing.allocator.free(stack);
        try testing.expectEqual(@as(i64, @bitCast(val)), stack[2]);
    }

    {
        const val: u64 = @bitCast(@as(i64, std.math.minInt(i64)) + 1);
        const lower: u32 = @truncate(val);
        const upper: u32 = @truncate(val >> 32);
        const tape = &.{
            @intFromEnum(Opcode.ldw),
            2,
            @as(i32, @bitCast(lower)),
            @as(i32, @bitCast(upper)),
        };

        const stack = try runTape(tape, 4, 0, 0);
        defer testing.allocator.free(stack);
        try testing.expectEqual(@as(i64, @bitCast(val)), stack[2]);
    }
}
