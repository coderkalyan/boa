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

const Node = Ast.Node;
const max_file_size = std.math.maxInt(u32);

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
    const module_slice = tree.extraData(tree.data(module_node).module.stmts, Node.ExtraSlice);
    const module_stmts = tree.extraSlice(module_slice);
    for (module_stmts) |stmt| {
        if (@as(Node.Tag, tree.data(stmt)) == .function) {
            const function = tree.data(stmt).function;

            // ig.lowerFunction(stmt);
            const ir = try IrGen.generate(gpa, &pool, &tree, function.body);
            {
                const ir_renderer = render.IrRenderer(2, @TypeOf(writer));
                var renderer = ir_renderer.init(writer, arena.allocator(), &ir);
                try renderer.render();
                try buffered_out.flush();
            }

            try writer.print("\n", .{});

            const bytecode = try Assembler.assemble(gpa, &pool, &ir);
            {
                const bytecode_renderer = render.BytecodeRenderer(2, @TypeOf(writer));
                var renderer = bytecode_renderer.init(writer, arena.allocator(), &bytecode);
                try renderer.render();
                try buffered_out.flush();
            }

            // try Interpreter.entry(gpa, &bytecode);
        }
    }
}
