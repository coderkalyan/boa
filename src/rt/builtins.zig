const std = @import("std");
const IrGen = @import("../ir/IrGen.zig");
const Assembler = @import("../bc/Assembler.zig");
const InternPool = @import("../InternPool.zig");
const String = @import("string.zig").String;
const ConstantPool = @import("ConstantPool.zig");

const Allocator = std.mem.Allocator;
const FunctionInfo = InternPool.FunctionInfo;

pub fn lazyCompileFunction(pool: *InternPool, constant_pool: *ConstantPool, fi: *FunctionInfo) !void {
    if (fi.lazy_ir == null) {
        const ir_data = try IrGen.generate(.function, pool.gpa, pool, fi.tree, fi.node);
        fi.lazy_ir = try pool.createIr(ir_data);
        // const ir = pool.irPtr(fi.lazy_ir.?);
        // {
        //     std.debug.print("ir listing for function:\n", .{});
        //     const ir_renderer = render.IrRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        //     // _ = ir_renderer;
        //     var renderer = ir_renderer.init(std.io.getStdOut().writer(), pool.gpa, ir);
        //     renderer.render() catch unreachable;
        // }
    }

    const ir = pool.irPtr(fi.lazy_ir.?);
    if (fi.lazy_bytecode == null) {
        const bc_data = Assembler.assemble(pool.gpa, pool, constant_pool, ir) catch unreachable;
        fi.lazy_bytecode = pool.createBytecode(bc_data) catch unreachable;
        // const bc = pool.bytecodePtr(fi.lazy_bytecode.?);
        // {
        //     std.debug.print("bytecode listing for function: {}\n", .{bc.code.len});
        //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        //     // _ = bytecode_renderer;
        //     var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), pool.gpa, pool, bc);
        //     renderer.render() catch unreachable;
        // }
    }
}
