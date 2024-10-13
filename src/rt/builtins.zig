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

const Context = types.Context;
const FunctionInfo = types.FunctionInfo;

export fn dispatch(id: u32, fp: [*]i64, sp: [*]i64, ctx: *anyopaque) callconv(.C) void {
    // _ = id;
    // _ = fp;
    // _ = sp;
    _ = ctx;
    std.debug.print("dispatch! {} {*} {*}\n", .{ id, fp, sp });
}

pub fn compile(ctx: *Context, fi: *FunctionInfo) callconv(.C) void {
    if (fi.state != .lazy) return;

    const ipool = ctx.ipool;
    const ir_data = IrGen.generate(.function, ctx.gpa, ipool, fi.tree, fi.node) catch unreachable;
    const ir_index = ipool.createIr(ir_data) catch unreachable;
    fi.ir = ipool.irPtr(ir_index);

    const bc_data = Assembler.assemble(ctx.gpa, ipool, fi.ir) catch unreachable;
    const bc_index = ipool.createBytecode(bc_data) catch unreachable;
    // TODO: this needs to be fixed
    fi.bytecode = ipool.bytecodePtr(bc_index).code.ptr;

    // {
    // std.debug.print("bytecode listing for function: {}\n", .{fi.bytecode.len});
    // const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
    // _ = bytecode_renderer;
    //     var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), ctx.gpa, ipool, fi.bytecode);
    //     renderer.render() catch unreachable;
    // }

    fi.state = .interpreted;
}

pub fn attrIndex(object: *Object, in_attr: u64) callconv(.C) i64 {
    const attr: InternPool.Index = @enumFromInt(@as(u32, @truncate(in_attr)));
    if (object.shape.get(attr)) |index| return @intCast(index);
    return -1;
}

pub fn attrLoad(object: *Object, index: u64) callconv(.C) i64 {
    return object.attributes.items[index];
}

pub fn attrStore(object: *Object, index: u64, val: i64) callconv(.C) void {
    object.attributes.items[index] = val;
}
