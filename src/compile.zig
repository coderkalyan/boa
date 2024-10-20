const std = @import("std");
const Ast = @import("Ast.zig");
const Ir = @import("ir/Ir.zig");
const IrGen = @import("ir/IrGen.zig");
const InternPool = @import("InternPool.zig");
const Bytecode = @import("bc/Bytecode.zig");
const Assembler = @import("bc/Assembler.zig");
const types = @import("rt/types.zig");
const render = @import("render.zig");

const Context = types.Context;
const FunctionInfo = types.FunctionInfo;

pub const CompilationInfo = struct {
    tree: *const Ast,
    ir: *const Ir,
    node: Ast.Node.Index,
    bytecode: *const Bytecode,
    state: State,

    pub const State = enum(u32) {
        lazy,
        interpreted,
        optimized_lite,
        optimized_full,
    };
};

pub fn compile(ctx: *Context, fi: *FunctionInfo) callconv(.C) void {
    const comp: *CompilationInfo = @ptrCast(@alignCast(fi.comp));
    if (comp.state != .lazy) return;

    const ipool: *InternPool = @ptrCast(@alignCast(ctx.ipool));
    const ir_data = IrGen.generate(.function, ctx.gpa, ipool, comp.tree, comp.node) catch unreachable;
    const ir_index = ipool.createIr(ir_data) catch unreachable;
    comp.ir = ipool.irPtr(ir_index);

    {
        const ir = comp.ir;
        const ir_renderer = render.IrRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        var renderer = ir_renderer.init(std.io.getStdOut().writer(), ctx.gpa, ir);
        renderer.render() catch unreachable;
    }

    const bc_data = Assembler.assemble(ctx.gpa, ipool, comp.ir) catch unreachable;
    const bc_index = ipool.createBytecode(bc_data) catch unreachable;
    // TODO: this needs to be fixed
    // TODO: but i forgot why
    comp.bytecode = ipool.bytecodePtr(bc_index);
    fi.bytecode = comp.bytecode.code.ptr;
    fi.frame_size = comp.bytecode.frame_size;

    {
        const code = ipool.bytecodePtr(bc_index);
        std.debug.print("bytecode listing for function: {}\n", .{code.code.len});
        const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        // _ = bytecode_renderer;
        var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), ctx.gpa, ipool, code);
        renderer.render() catch unreachable;
    }

    comp.state = .interpreted;
}
