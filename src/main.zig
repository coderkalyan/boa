const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lex.zig").Lexer;
const Ast = @import("Ast.zig");
const parse = @import("parse.zig");
const IrGen = @import("ir/IrGen.zig");
const Assembler = @import("bc/Assembler.zig");
const InternPool = @import("InternPool.zig");
const render = @import("render.zig");
// const Interpreter = @import("rt/Interpreter.zig");
const Bytecode = @import("bc/Bytecode.zig");
const Object = @import("rt/object.zig").Object;
const Shape = @import("rt/Shape.zig");
const PageBumpAllocator = @import("PageBumpAllocator.zig");
const types = @import("rt/types.zig");
// const builtins = @import("rt/builtins.zig");
// const builtins = @import("interpreter/builtins.zig");
// const builtins_impl = @import("rt/builtins_impl.zig");
const builtins = @import("rt/builtins.zig");

const posix = std.posix;
const Node = Ast.Node;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const max_file_size = std.math.maxInt(u32);
const value_stack_size = 8 * 1024 * 1024;
const call_stack_size = 1 * 1024 * 1024;

extern fn interpreter_entry(ip: [*]i32, fp: [*]i64, sp: [*]i64, ctx: *types.Context) callconv(.C) void;

pub fn readSource(gpa: Allocator, input_filename: []const u8) ![:0]u8 {
    var file = try std.fs.cwd().openFile(input_filename, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    const source = try gpa.allocSentinel(u8, @intCast(stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.process.exit(1);
    }

    return source;
}

pub fn main() !void {
    var allocator: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const gpa = allocator.allocator();
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();

    _ = args.next(); // skip executable
    const filename = args.next();

    const source: [:0]const u8 = try readSource(gpa, filename.?);

    const out = std.io.getStdOut();
    var buffered_out = std.io.bufferedWriter(out.writer());
    const writer = buffered_out.writer();

    const tree = try parse.parse(gpa, source);
    var pool = try InternPool.init(gpa);

    // post order format guarantees that the module node will be the last
    const module_node: Node.Index = @enumFromInt(@as(u32, @intCast(tree.nodes.len - 1)));
    const ir_data = try IrGen.generate(.module, gpa, &pool, &tree, module_node);
    const ir_index = try pool.createIr(ir_data);
    const ir = pool.irPtr(ir_index);

    // {
    //     const ir_renderer = render.IrRenderer(2, @TypeOf(writer));
    //     // _ = ir_renderer;
    //     var renderer = ir_renderer.init(writer, arena.allocator(), ir);
    //     try renderer.render();
    //     try buffered_out.flush();
    // }

    try writer.print("\n", .{});

    const bc_data = try Assembler.assemble(gpa, &pool, ir);
    const bc_index = try pool.createBytecode(bc_data);
    // const bc = pool.bytecodePtr(bc_index);
    // {
    //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(writer));
    //     // _ = bytecode_renderer;
    //     var renderer = bytecode_renderer.init(writer, arena.allocator(), &pool, bc);
    //     renderer.render() catch |err| {
    //         try buffered_out.flush();
    //         return err;
    //     };
    //     try buffered_out.flush();
    // }

    const findex = try pool.createFunction(.{
        .state = .interpreted,
        .tree = &tree,
        .node = module_node,
        .return_type = .nonetype,
        .ir = ir_index,
        .bytecode = bc_index,
    });
    const ip = try pool.put(.{ .function = findex });

    try interpret(gpa, &pool, ip);
}

pub fn interpret(
    gpa: Allocator,
    pool: *InternPool,
    fi_ip: InternPool.Index,
) !void {
    var page_bump: PageBumpAllocator = .{};
    const pba = page_bump.allocator();

    const shape_ptr = try pba.create(Shape);
    shape_ptr.* = try Shape.init(pba);
    const global = try Object.init(gpa, shape_ptr);
    defer gpa.destroy(global);

    const stack = try posix.mmap(
        null,
        value_stack_size,
        posix.PROT.READ | posix.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    const function = pool.functionPtr(pool.get(fi_ip).function);
    const fi_ptr = try pba.create(types.FunctionInfo);
    fi_ptr.* = .{
        .tree = function.tree,
        .ir = pool.irPtr(function.ir),
        .bytecode = pool.bytecodePtr(function.bytecode).code.ptr,
        .node = function.node,
        .frame_size = pool.bytecodePtr(function.bytecode).frame_size,
        .state = .interpreted,
    };

    const fi_ldw: i64 = @bitCast(@intFromPtr(fi_ptr));
    std.debug.print("0x{x}\n", .{fi_ldw});
    var code: std.ArrayListUnmanaged(i32) = .{};
    try code.appendSlice(gpa, &.{
        // ldw x0, fi_ptr
        @intFromEnum(Opcode.ldw),
        0,
        @truncate(fi_ldw),
        @truncate(fi_ldw >> 32),
        // call x0, x0
        @intFromEnum(Opcode.call_init),
        0,
        0,
        0, // inline cache entry
        @intFromEnum(Opcode.exit),
    });
    const entry_bc: Bytecode = .{
        .frame_size = 1,
        .code = code.items,
    };

    const fp: [*]i64 = @ptrCast(@alignCast(stack.ptr));
    const sp = fp + entry_bc.frame_size;
    std.debug.print("{*} {*}\n", .{ fp, sp });

    var context: types.Context = .{
        .ipool = pool,
        .global = global,
        .gpa = gpa,
        .pba = pba,
        .gca = undefined, // TODO: put this in
    };

    interpreter_entry(entry_bc.code.ptr, fp, sp, &context);
    std.debug.print("{any}\n", .{fp[0..16]});
    // std.debug.print("{x}\n", .{fp[entry_bc.frame_size + 4 + 0]});
}

comptime {
    @export(builtins.compile, .{ .name = "rt_compile", .linkage = .strong });
    @export(builtins.dispatch, .{ .name = "rt_dispatch", .linkage = .strong });
    @export(builtins.attrIndex, .{ .name = "rt_attr_index", .linkage = .strong });
    @export(builtins.attrInsert, .{ .name = "rt_attr_insert", .linkage = .strong });
    @export(builtins.attrLoad, .{ .name = "rt_attr_load", .linkage = .strong });
    @export(builtins.attrStore, .{ .name = "rt_attr_store", .linkage = .strong });
    // @export(builtins_impl.pushArgs, .{ .name = "rt_push_args", .linkage = .strong });
    // @export(builtins_impl.evalCallable, .{ .name = "rt_eval_callable", .linkage = .strong });
    // @export(builtins_impl.trap, .{ .name = "rt_trap", .linkage = .strong });
    // @export(builtins_impl.attrIndexOrPanic, .{ .name = "rt_attr_index_or_panic", .linkage = .strong });
    // @export(builtins_impl.attrIndexOrInsert, .{ .name = "rt_attr_index_or_insert", .linkage = .strong });
    // @export(builtins_impl.loadIndex, .{ .name = "rt_load_index", .linkage = .strong });
    // @export(builtins_impl.storeIndex, .{ .name = "rt_store_index", .linkage = .strong });
    // @export(builtins_impl.pint, .{ .name = "rt_pint", .linkage = .strong });
}
