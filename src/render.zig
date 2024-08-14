const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Ir = @import("ir/Ir.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;
const Allocator = std.mem.Allocator;

const io = std.io;

const Node = Ast.Node;

pub fn AstRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        tree: *const Ast,

        pub const Self = @This();

        pub fn init(writer: anytype, tree: *const Ast) Self {
            return .{ .stream = indentingWriter(width, writer), .tree = tree };
        }

        pub fn render(self: *Self) !void {
            try self.renderNode(@intCast(self.tree.nodes.len - 1));
        }

        pub fn renderNode(self: *Self, node: Node.Index) !void {
            const tree = self.tree;
            var writer = self.stream.writer();

            switch (tree.nodes.items(.data)[node]) {
                .placeholder => {},
                .toplevel => |toplevel| {
                    var stmt = toplevel.stmts_start;
                    while (stmt < toplevel.stmts_end) : (stmt += 1) {
                        try self.renderNode(tree.extra_data[stmt]);
                        try writer.writeAll(";");
                        try self.stream.newline();
                    }
                },
                .named_ty => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                },
                .fn_decl => |decl| {
                    try writer.writeAll("fn ");

                    try writer.writeAll("(");
                    const signature = tree.extraData(decl.signature, Node.FnSignature);
                    if (signature.params_end > signature.params_start) {
                        var param: u32 = signature.params_start;
                        while (param < signature.params_end - 1) : (param += 1) {
                            try self.renderNode(tree.extra_data[param]);
                            try writer.writeAll(", ");
                        }
                        try self.renderNode(tree.extra_data[param]);
                    }
                    try writer.writeAll(") ");
                    try self.renderNode(signature.return_ty);

                    try writer.writeAll(" ");
                    try self.renderNode(decl.body);
                },
                .param => |param| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(": ");
                    try self.renderNode(param.ty);
                },
                .integer_literal, .float_literal, .bool_literal => {
                    const literal = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(literal);
                },
                .binary_expr => |expr| {
                    try writer.writeAll("(");
                    try self.renderNode(expr.left);

                    try writer.writeAll(" ");
                    const operator = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(operator);
                    try writer.writeAll(" ");

                    try self.renderNode(expr.right);
                    try writer.writeAll(")");
                },
                .var_expr => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                },
                .call_expr => |call| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);

                    try writer.writeAll("(");
                    if (call.args_end > call.args_start) {
                        var arg = call.args_start;
                        while (arg < call.args_end - 1) : (arg += 1) {
                            try self.renderNode(tree.extra_data[arg]);
                            try writer.writeAll(", ");
                        }
                        try self.renderNode(tree.extra_data[arg]);
                    }
                    try writer.writeAll(")");
                },
                .ty_decl => |decl| {
                    try writer.writeAll("type ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.ty);
                    try self.stream.newline();
                },
                .block => |block| {
                    try writer.writeAll("{");
                    self.stream.indent();
                    try self.stream.newline();

                    var stmt = block.stmts_start;
                    while (stmt < block.stmts_end) : (stmt += 1) {
                        try self.renderNode(tree.extra_data[stmt]);
                        switch (tree.nodes.items(.data)[tree.extra_data[stmt]]) {
                            .ty_decl, .const_decl, .var_decl, .return_val, .assign_simple, .assign_binary => {
                                try writer.writeAll(";");
                                try self.stream.newline();
                            },
                            else => {},
                        }
                    }

                    self.stream.dedent();
                    try writer.writeAll("}");
                },
                .const_decl => |decl| {
                    try writer.writeAll("let ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .var_decl => |decl| {
                    try writer.writeAll("let mut ");
                    const ident = tree.tokenString(tree.mainToken(node) + 2);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .return_val => |ret| {
                    try writer.writeAll("return ");
                    try self.renderNode(ret.val);
                },
                .if_simple => |data| {
                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(data.exec_true);
                    try self.stream.newline();
                },
                .if_else => |data| {
                    const exec = tree.extraData(data.exec, Node.IfElse);

                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(exec.exec_true);
                    try writer.writeAll(" else ");
                    try self.renderNode(exec.exec_false);
                    try self.stream.newline();
                },
                .if_chain => |data| {
                    const chain = tree.extraData(data.chain, Node.IfChain);

                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(chain.exec_true);
                    try writer.writeAll(" else ");
                    try self.renderNode(chain.next);
                },
                .loop_forever => |loop| {
                    try writer.writeAll("for ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .loop_conditional => |loop| {
                    try writer.writeAll("for ");
                    try self.renderNode(loop.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .loop_range => |loop| {
                    const signature = tree.extraData(loop.signature, Node.RangeSignature);

                    try writer.writeAll("for ");
                    try self.renderNode(signature.binding);
                    try writer.writeAll("; ");
                    try self.renderNode(signature.condition);
                    try writer.writeAll("; ");
                    try self.renderNode(signature.afterthought);
                    try writer.writeAll(" ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .assign_simple => |assign| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(assign.val);
                },
                .assign_binary => |assign| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    const operator = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" ");
                    try writer.writeAll(operator);
                    try writer.writeAll(" ");
                    try self.renderNode(assign.val);
                },
            }
        }
    };
}

pub fn IrRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        ir: *const Ir,
        arena: Allocator,

        pub const Self = @This();

        pub fn init(writer: anytype, arena: Allocator, ir: *const Ir) Self {
            return .{
                .stream = indentingWriter(width, writer),
                .arena = arena,
                .ir = ir,
            };
        }

        pub fn render(self: *Self) !void {
            const ir = self.ir;

            const slice = ir.extraData(Ir.Inst.ExtraSlice, ir.block);
            const insts = ir.extraSlice(slice);
            for (insts) |inst| {
                try self.renderInst(@enumFromInt(inst));
            }
        }

        pub fn renderInst(self: *Self, inst: Ir.Index) !void {
            const ir = self.ir;
            const index = @intFromEnum(inst);
            const writer = self.stream.writer();
            try writer.print("%{} = ", .{index});

            const tag = ir.insts.items(.tag)[index];
            const payload = ir.insts.items(.payload)[index];
            switch (tag) {
                // .block => {
                //     try writer.print("block {{", .{});
                //     self.stream.indent();
                //     try self.stream.newline();
                //
                //     const slice = ir.extraData(Ir.Inst.ExtraSlice, block.insts);
                //     const insts = ir.extraSlice(slice);
                //     for (insts) |block_inst| {
                //         try self.renderInst(@enumFromInt(block_inst));
                //     }
                //
                //     self.stream.dedent();
                //     try writer.print("}}", .{});
                //     try self.stream.newline();
                // },
                // .branch_single => |branch_single| {
                //     const cond = @intFromEnum(branch_single.cond);
                //     try writer.print("branch_single(%{}) true: ", .{cond});
                //     try self.renderInst(branch_single.exec_true);
                // },
                // .branch_double => |data| {
                //     const cond = @intFromEnum(data.cond);
                //     const branch_double = ir.extraData(Ir.Inst.BranchDouble, data.pl);
                //     try writer.print("branch_double(%{}) {{", .{cond});
                //     self.stream.indent();
                //     try self.stream.newline();
                //     try writer.print("true: ", .{});
                //     try self.renderInst(branch_double.exec_true);
                //     try writer.print("false: ", .{});
                //     try self.renderInst(branch_double.exec_false);
                //     self.stream.dedent();
                //     try writer.print("}}", .{});
                //     try self.stream.newline();
                // },
                // .loop_while => |loop| {
                //     try writer.print("loop_while {{", .{});
                //     self.stream.indent();
                //     try self.stream.newline();
                //     try writer.print("condition: ", .{});
                //     try self.renderInst(loop.cond);
                //     try writer.print("body: ", .{});
                //     try self.renderInst(loop.body);
                //     self.stream.dedent();
                //     try writer.print("}}", .{});
                //     try self.stream.newline();
                // },
                // .call => |call| {
                //     try writer.print("call(%{}", .{@intFromEnum(call.function)});
                //     const slice = ir.extraData(Ir.Inst.ExtraSlice, call.args);
                //     const args = ir.extraSlice(slice);
                //     for (args) |arg| {
                //         try writer.print(", %{}", .{arg});
                //     }
                //     try writer.print(")", .{});
                //     try self.stream.newline();
                // },
                inline else => {
                    try writer.print("{s}(", .{@tagName(tag)});

                    switch (Ir.payloadTag(tag)) {
                        .placeholder => {},
                        .unary => try writer.print("%{}", .{@intFromEnum(payload.unary)}),
                        .binary => {
                            const l = @intFromEnum(payload.binary.l);
                            const r = @intFromEnum(payload.binary.r);
                            try writer.print("%{}, %{}", .{ l, r });
                        },
                        .ip => try ir.pool.print(writer, payload.ip),
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
            }
        }
    };
}

fn IndentingWriter(comptime width: u32, comptime WriterType: type) type {
    return struct {
        depth: u32,
        underlying_writer: WriterType,
        needs_indent: bool,

        const Self = @This();
        pub const Error = WriterType.Error;
        pub const Writer = io.Writer(*Self, Error, write);

        pub fn newline(self: *Self) !void {
            if (self.needs_indent) try self.writeIndent();
            try self.underlying_writer.writeAll("\n");
            self.needs_indent = true;
        }

        pub fn indent(self: *Self) void {
            self.depth += 1;
        }

        pub fn dedent(self: *Self) void {
            std.debug.assert(self.depth >= 1);
            self.depth -= 1;
        }

        fn writeIndent(self: *Self) !void {
            self.needs_indent = false;

            var i: u32 = 0;
            while (i < self.depth) : (i += 1) {
                try self.underlying_writer.writeAll(" " ** width);
            }
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (self.needs_indent) try self.writeIndent();
            return self.underlying_writer.write(bytes);
        }
    };
}

fn indentingWriter(comptime width: u32, underlying_stream: anytype) IndentingWriter(width, @TypeOf(underlying_stream)) {
    return .{ .depth = 0, .underlying_writer = underlying_stream, .needs_indent = false };
}
