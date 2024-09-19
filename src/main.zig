const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lex.zig").Lexer;
const Ast = @import("Ast.zig");
const parse = @import("parse.zig");
const IrGen = @import("ir/IrGen.zig");
const Assembler = @import("bc/Assembler.zig");
const InternPool = @import("InternPool.zig");
const render = @import("render.zig");
const Interpreter = @import("rt/Interpreter.zig");
const Bytecode = @import("bc/Bytecode.zig");
const Object = @import("rt/object.zig").Object;
const ShapePool = @import("rt/ShapePool.zig");
const ConstantPool = @import("rt/ConstantPool.zig");
const PageBumpAllocator = @import("PageBumpAllocator.zig");

const posix = std.posix;
const Node = Ast.Node;
const Shape = ShapePool.Shape;
const asBytes = std.mem.asBytes;
const max_file_size = std.math.maxInt(u32);
const stack_size = 8 * 1024 * 1024;

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
    // _ = writer;

    // var lexer = try Lexer.init(source, arena.allocator());
    // while (true) {
    //     const token = try lexer.next(arena.allocator());
    //     // std.debug.print("{s} '{s}'\n", .{ @tagName(token.tag), source[token.loc.start..token.loc.end] });
    //     if (token.tag == .eof) break;
    //     if (token.tag == .newline) std.debug.print("\n", .{});
    // }

    const tree = try parse.parse(gpa, source);
    // for (tree.nodes.items(.data), tree.nodes.items(.main_token)) |data, tok| {
    //     std.debug.print("{} {}\n", .{ data, tree.tokens.items(.tag)[tok] });
    // }
    var page_bump1: PageBumpAllocator = .{};
    const pba1 = page_bump1.allocator();
    var pool = try InternPool.init(pba1);

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

    var page_bump: PageBumpAllocator = .{};
    const pba = page_bump.allocator();
    var constant_pool = ConstantPool.init(gpa, pba);

    const bc_data = try Assembler.assemble(gpa, &pool, &constant_pool, ir);
    const bc_index = try pool.createBytecode(bc_data);
    // const bc = pool.bytecodePtr(bc_index);
    // {
    //     const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(writer));
    //     // _ = bytecode_renderer;
    //     var renderer = bytecode_renderer.init(writer, arena.allocator(), &pool, bc);
    //     try renderer.render();
    //     try buffered_out.flush();
    // }

    const findex = try pool.createFunction(.{
        .tree = &tree,
        .node = module_node,
        .lazy_ir = ir_index,
        .lazy_bytecode = bc_index,
        .return_type = .nonetype,
    });
    const ip = try pool.put(.{ .function = findex });

    try interpret(gpa, &pool, &constant_pool, ip);
}

pub fn interpret(
    gpa: Allocator,
    pool: *InternPool,
    constant_pool: *ConstantPool,
    fi_ip: InternPool.Index,
) !void {
    var shape_pool = try ShapePool.init(gpa);
    const shape = try shape_pool.createShape();
    const shape_ptr = shape_pool.shapePtr(shape);

    const global = try Object.init(gpa, shape_ptr);
    defer gpa.destroy(global);

    const stack_memory = try posix.mmap(
        null,
        stack_size,
        posix.PROT.READ | posix.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );
    const stack: [*]Interpreter.Slot = @ptrCast(stack_memory.ptr);

    const pool_ptr: usize = @intFromPtr(pool);
    const fi_ptr = pool.functionPtr(pool.get(fi_ip).function);
    const constants: []const *anyopaque = &.{fi_ptr};
    const ptr: usize = @intFromPtr(constants.ptr);
    const entry_bc: Bytecode = .{
        .register_count = 0,
        .ic_count = 0,
        .code = &.{
            .{ .imm = @truncate(pool_ptr) },
            .{ .imm = @truncate(pool_ptr >> 32) },
            .{ .imm = @truncate(ptr) },
            .{ .imm = @truncate(ptr >> 32) },
            .{ .opcode = .ldi },
            .{ .register = 0 },
            .{ .count = 0 },
            .{ .opcode = .call },
            .{ .register = 0 },
            .{ .register = 0 },
            .{ .count = 0 },
            .{ .opcode = .exit },
        },
        .entry_pc = 2,
    };

    const ic_vector = try gpa.alloc(u32, 10);
    @memset(ic_vector, std.math.maxInt(u32));
    var page_bump: PageBumpAllocator = .{};
    const pba = page_bump.allocator();

    var context_frame: Interpreter.ContextFrame = .{
        .pba = &pba,
        .global_object = global,
        .shape_pool = &shape_pool,
        .constant_pool = constant_pool,
        .ic_vector = ic_vector.ptr,
    };

    const context_slots = std.meta.fields(Interpreter.ContextFrame).len;
    @memcpy(asBytes(stack[0..context_slots]), asBytes(&context_frame));

    const fp = context_slots;
    const sp = fp;
    Interpreter.entry(4, entry_bc.code.ptr, fp, sp, stack);
}
