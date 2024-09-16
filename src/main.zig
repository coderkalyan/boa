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

const posix = std.posix;
const Node = Ast.Node;
const GlobalMap = Interpreter.GlobalMap;
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
    var pool = try InternPool.init(gpa);

    // post order format guarantees that the module node will be the last
    const module_node: u32 = @intCast(tree.nodes.len - 1);
    const ir_data = try IrGen.generate(.module, gpa, &pool, &tree, module_node);
    const ir_index = try pool.createIr(ir_data);
    const ir = pool.irPtr(ir_index);

    {
        const ir_renderer = render.IrRenderer(2, @TypeOf(writer));
        // _ = ir_renderer;
        var renderer = ir_renderer.init(writer, arena.allocator(), ir);
        try renderer.render();
        try buffered_out.flush();
    }

    try writer.print("\n", .{});

    const bc_data = try Assembler.assemble(gpa, &pool, ir);
    const bc_index = try pool.createBytecode(bc_data);
    const bc = pool.bytecodePtr(bc_index);
    {
        const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(writer));
        // _ = bytecode_renderer;
        var renderer = bytecode_renderer.init(writer, arena.allocator(), &pool, bc);
        try renderer.render();
        try buffered_out.flush();
    }

    const findex = try pool.createFunction(.{
        .tree = &tree,
        .node = module_node,
        .lazy_ir = ir_index,
        .lazy_bytecode = bc_index,
    });
    const ip = try pool.put(.{ .function = findex });

    try interpret(gpa, &pool, ip);
}

pub fn interpret(gpa: Allocator, pool: *InternPool, fi_ip: InternPool.Index) !void {
    var context = GlobalMap.init(gpa);
    defer context.deinit();

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
    const entry_bc: Bytecode = .{
        .register_count = 0,
        .code = &.{
            .{ .imm = @truncate(pool_ptr) },
            .{ .imm = @truncate(pool_ptr >> 32) },
            .{ .opcode = .ldi },
            .{ .register = 0 },
            .{ .ip = fi_ip },
            .{ .opcode = .call },
            .{ .register = 0 },
            .{ .count = 0 },
            .{ .opcode = .exit },
        },
    };

    stack[0].ptr = &context;
    Interpreter.entry(2, entry_bc.code.ptr, 1, 1, stack);
}
