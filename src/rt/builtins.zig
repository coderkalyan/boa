const std = @import("std");
const Ast = @import("../Ast.zig");
const Ir = @import("../ir/Ir.zig");
const IrGen = @import("../ir/IrGen.zig");
const InternPool = @import("../InternPool.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const Assembler = @import("../bc/Assembler.zig");
const types = @import("types.zig");
const render = @import("../render.zig");
const Object = @import("object.zig").Object;
const String = @import("string.zig").String;

const Context = types.Context;
const FunctionInfo = types.FunctionInfo;

pub const Id = enum(i32) {
    print1_int,
    print1_float,
    print1_bool,
    print1_str,
    ipow,
    fpow,
    strcat,
    strrep,
    strlen,
};

inline fn arg(sp: [*]i64, index: usize) i64 {
    return (sp - 1 - index)[0];
}

export fn dispatch(in_id: u32, ret: *i64, sp: [*]i64, ctx: *Context) callconv(.C) void {
    const id: Id = @enumFromInt(in_id);
    switch (id) {
        .print1_int => print1Int(arg(sp, 0)),
        .print1_float => print1Float(@bitCast(arg(sp, 0))),
        .print1_bool => print1Bool(@bitCast(arg(sp, 0))),
        .print1_str => print1Str(@ptrFromInt(@as(u64, @bitCast(arg(sp, 0))))),
        .ipow => {
            const base = arg(sp, 0);
            const exponent = arg(sp, 1);
            ret.* = ipow(base, exponent);
        },
        .fpow => {
            const base: f64 = @bitCast(arg(sp, 0));
            const exponent: f64 = @bitCast(arg(sp, 1));
            ret.* = @bitCast(fpow(base, exponent));
        },
        .strcat => {
            const l: *const String = @ptrFromInt(@as(u64, @bitCast(arg(sp, 0))));
            const r: *const String = @ptrFromInt(@as(u64, @bitCast(arg(sp, 1))));
            const cat = String.catenate(ctx.pba, l, r) catch unreachable;
            ret.* = @bitCast(@intFromPtr(cat));
        },
        .strrep => {
            const template: *const String = @ptrFromInt(@as(u64, @bitCast(arg(sp, 0))));
            const count: u64 = @bitCast(arg(sp, 1));
            const rep = String.repeat(ctx.pba, template, count) catch unreachable;
            ret.* = @bitCast(@intFromPtr(rep));
        },
        .strlen => {
            // TODO: we shouldn't need runtime to do this, just string
            // introspection in the vm
            const op: *const String = @ptrFromInt(@as(u64, @bitCast(arg(sp, 0))));
            ret.* = @intCast(op.len);
        },
    }
}

pub fn compile(ctx: *Context, fi: *FunctionInfo) callconv(.C) void {
    if (fi.state != .lazy) return;

    const ipool = ctx.ipool;
    const ir_data = IrGen.generate(.function, ctx.gpa, ipool, fi.tree, fi.node) catch unreachable;
    const ir_index = ipool.createIr(ir_data) catch unreachable;
    fi.ir = ipool.irPtr(ir_index);

    // {
    //     const ir = fi.ir;
    //     const ir_renderer = render.IrRenderer(2, @TypeOf(std.io.getStdOut().writer()));
    //     var renderer = ir_renderer.init(std.io.getStdOut().writer(), ctx.gpa, ir);
    //     renderer.render() catch unreachable;
    // }

    const bc_data = Assembler.assemble(ctx.gpa, ipool, fi.ir) catch unreachable;
    const bc_index = ipool.createBytecode(bc_data) catch unreachable;
    // TODO: this needs to be fixed
    // TODO: but i forgot why
    fi.bytecode = ipool.bytecodePtr(bc_index).code.ptr;
    fi.frame_size = ipool.bytecodePtr(bc_index).frame_size;

    // {
    //     const code = ipool.bytecodePtr(bc_index);
    //     std.debug.print("bytecode listing for function: {}\n", .{code.code.len});
    //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
    //     // _ = bytecode_renderer;
    //     var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), ctx.gpa, ipool, code);
    //     renderer.render() catch unreachable;
    // }

    fi.state = .interpreted;
}

pub fn attrIndex(object: *Object, in_attr: u64) callconv(.C) i64 {
    const attr: InternPool.Index = @enumFromInt(@as(u32, @truncate(in_attr)));
    if (object.shape.get(attr)) |index| return @intCast(index);
    return -1;
}

pub fn attrInsert(ctx: *Context, object: *Object, in_attr: u64) callconv(.C) i64 {
    const attr: InternPool.Index = @enumFromInt(@as(u32, @truncate(in_attr)));
    object.shape = object.shape.transition(ctx.pba, attr) catch unreachable;
    _ = object.attributes.addOne(ctx.pba) catch unreachable;
    return @intCast(object.shape.get(attr).?);
}

pub fn attrLoad(object: *Object, index: u64) callconv(.C) i64 {
    return object.attributes.items[index];
}

pub fn attrStore(object: *Object, index: u64, val: i64) callconv(.C) void {
    object.attributes.items[index] = val;
}

pub fn print1Int(int: i64) void {
    std.debug.print("{}\n", .{int});
}

pub fn print1Float(float: f64) void {
    std.debug.print("{}\n", .{float});
}

pub fn print1Bool(b: u64) void {
    std.debug.assert(b <= 1);
    const table: []const []const u8 = &.{ "False", "True" };
    std.debug.print("{s}\n", .{table[b]});
}

pub fn print1Str(str: *const String) void {
    std.debug.print("{s}\n", .{str.bytes()});
}

pub fn ipow(base: i64, exponent: i64) i64 {
    return std.math.pow(i64, base, exponent);
}

pub fn fpow(base: f64, exponent: f64) f64 {
    return std.math.pow(f64, base, exponent);
}
