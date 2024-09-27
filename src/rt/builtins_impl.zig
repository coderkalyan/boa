const std = @import("std");
const IrGen = @import("../ir/IrGen.zig");
const Assembler = @import("../bc/Assembler.zig");
const InternPool = @import("../InternPool.zig");
const String = @import("string.zig").String;
const ConstantPool = @import("ConstantPool.zig");
const Interpreter = @import("Interpreter.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const PageBumpAllocator = @import("../PageBumpAllocator.zig");
const Object = @import("../rt/object.zig").Object;
const Context = @import("../interpreter/builtins.zig").Context;
const render = @import("../render.zig");

const Allocator = std.mem.Allocator;
const FunctionInfo = InternPool.FunctionInfo;
const Slot = Interpreter.Slot;
const Word = Bytecode.Word;
const asBytes = std.mem.asBytes;
const Handler = *const fn (ip: [*]const i32, fp: [*]i64, sp: [*]i64) void;

// read `count` bytecode words starting at `start`, interpret them as registers,
// load values from the current stack frame (fp) and push to top of stack (sp)
// in reverse order
pub fn pushArgs(start: [*]const i32, count: u64, fp: [*]i64, sp: [*]i64) callconv(.C) void {
    var i = count;
    var dst = sp;
    while (i > 0) {
        i -= 1;
        const register = start[i];
        const base: usize = @intCast(@as(i128, @intFromPtr(fp)) + @sizeOf(i64) * register);
        const src: [*]i64 = @ptrFromInt(base);
        // std.debug.print("pushing {}\n", .{register});
        @memcpy(asBytes(dst)[0..8], asBytes(src)[0..8]);
        dst += 1;
    }
}

extern fn interpreter_trampoline(ip: [*]const i32, fp: [*]i64, sp: [*]i64) void;

// lookup the FunctionInfo pointer, lazily compile the function, and return
// a jump target
pub fn evalCallable(ctx: *Context, fi_ptr: *FunctionInfo, sp: [*]i64) callconv(.C) *const anyopaque {
    // std.debug.print("{}\n", .{fi_ptr});
    const intern_pool = fi_ptr.intern_pool;
    var constant_pool = ConstantPool.init(intern_pool.gpa, ctx.pba);
    const bc = lazyCompileFunction(intern_pool, &constant_pool, fi_ptr) catch unreachable;
    // std.debug.print("{} {*}\n", .{ bc, bc.code.ptr + 2 });
    // std.debug.print("{}\n", .{(bc.code.ptr + 2)[0].opcode});
    sp[0] = @bitCast(@intFromPtr(bc.code.ptr + 2));
    sp[1] = bc.register_count;
    return interpreter_trampoline;
}

pub fn trap(opcode: Bytecode.Opcode) callconv(.C) void {
    std.debug.print("trap: {}\n", .{opcode});
    while (true) {}
}

pub fn attrIndexOrPanic(ctx: *Context, attr: InternPool.Index) callconv(.C) usize {
    return ctx.global_object.shape.get(attr).?;
}

pub fn attrIndexOrInsert(ctx: *Context, attr: InternPool.Index) callconv(.C) usize {
    const global = ctx.global_object;
    if (global.shape.get(attr)) |index| return index;
    global.shape = global.shape.transition(ctx.pba, attr) catch unreachable;
    _ = global.attributes.addOne(ctx.pba) catch unreachable;
    return ctx.global_object.shape.get(attr).?;
}

pub fn loadIndex(ctx: *Context, index: usize) callconv(.C) i64 {
    return ctx.global_object.attributes.items[index];
}

pub fn storeIndex(ctx: *Context, index: usize, value: i64) callconv(.C) void {
    ctx.global_object.attributes.items[index] = value;
}

pub fn lazyCompileFunction(pool: *InternPool, constant_pool: *ConstantPool, fi: *FunctionInfo) !*const Bytecode {
    if (fi.lazy_bytecode == null) {
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
        const bc_data = Assembler.assemble(pool.gpa, pool, constant_pool, ir) catch unreachable;
        fi.lazy_bytecode = pool.createBytecode(bc_data) catch unreachable;
        // std.debug.print("compile!\n", .{});
        // const bc = pool.bytecodePtr(fi.lazy_bytecode.?);
        // {
        //     std.debug.print("bytecode listing for function: {}\n", .{bc.code.len});
        //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(std.io.getStdOut().writer()));
        //     // _ = bytecode_renderer;
        //     var renderer = bytecode_renderer.init(std.io.getStdOut().writer(), pool.gpa, pool, bc);
        //     renderer.render() catch unreachable;
        // }
    }

    const bc = pool.bytecodePtr(fi.lazy_bytecode.?);
    return bc;
}
