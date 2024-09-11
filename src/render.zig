const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Ir = @import("ir/Ir.zig");
const Bytecode = @import("bc/Bytecode.zig");
const InternPool = @import("InternPool.zig");
const Liveness = @import("ir/Liveness.zig");
const Type = @import("type.zig").Type;
const Allocator = std.mem.Allocator;

const io = std.io;
const asBytes = std.mem.asBytes;

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
            const writer = self.stream.writer();
            for (self.ir.blocks, 0..) |block, i| {
                try writer.print("block{}:", .{i});
                try self.stream.newline();

                self.stream.indent();
                try self.renderBlock(block);
                self.stream.dedent();
            }
        }

        pub fn renderBlock(self: *Self, slice: Ir.Inst.ExtraSlice) WriterType.Error!void {
            const ir = self.ir;

            const insts = ir.extraSlice(slice);
            for (insts) |inst| {
                try self.renderInst(@enumFromInt(inst));
            }
        }

        pub fn renderInst(self: *Self, inst: Ir.Index) !void {
            const ir = self.ir;
            const index = @intFromEnum(inst);
            const writer = self.stream.writer();
            const dead_bits = ir.liveness.deadBits(inst);

            if (dead_bits & 0x8 != 0) {
                try writer.print("!%{} = ", .{index});
            } else {
                try writer.print("%{} = ", .{index});
            }

            const tag = ir.insts.items(.tag)[index];
            const payload = ir.insts.items(.payload)[index];
            switch (tag) {
                .br => {
                    const branch = ir.extraData(Ir.Inst.Branch, payload.unary_extra.extra);
                    const cond = @intFromEnum(payload.unary_extra.op);
                    const exec_if = @intFromEnum(branch.exec_if);
                    const exec_else = @intFromEnum(branch.exec_else);
                    try writer.print("br(%{}, block{}, block{})", .{ cond, exec_if, exec_else });
                    try self.stream.newline();
                },
                .phi => {
                    const phi = ir.extraData(Ir.Inst.Phi, payload.extra);
                    try writer.print("phi(", .{});
                    try ir.pool.print(writer, phi.ty);
                    const src1 = @intFromEnum(phi.src1);
                    const src2 = @intFromEnum(phi.src2);
                    try writer.print(", %{}, %{})", .{ src1, src2 });
                    try self.stream.newline();
                },
                inline else => {
                    try writer.print("{s}(", .{@tagName(tag)});

                    switch (Ir.payloadTag(tag)) {
                        .placeholder => {},
                        .unary => if (dead_bits & 0x1 != 0) {
                            try writer.print("!%{}", .{@intFromEnum(payload.unary)});
                        } else {
                            try writer.print("%{}", .{@intFromEnum(payload.unary)});
                        },
                        .binary => {
                            const l = @intFromEnum(payload.binary.l);
                            const dead_l = if (dead_bits & 0x1 != 0) "!" else "";
                            const r = @intFromEnum(payload.binary.r);
                            const dead_r = if (dead_bits & 0x2 != 0) "!" else "";
                            try writer.print("{s}%{}, {s}%{}", .{ dead_l, l, dead_r, r });
                        },
                        .ip => try ir.pool.print(writer, payload.ip),
                        .block => try writer.print("block{}", .{@intFromEnum(payload.block)}),
                        .extra, .unary_extra => unreachable,
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
            }
        }
    };
}

pub fn BytecodeRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        bc: *const Bytecode,
        arena: Allocator,

        pub const Self = @This();

        pub fn init(writer: anytype, arena: Allocator, bc: *const Bytecode) Self {
            return .{
                .stream = indentingWriter(width, writer),
                .arena = arena,
                .bc = bc,
            };
        }

        pub fn render(self: *Self) !void {
            for (0..self.bc.code.len) |i| {
                try self.renderInst(@intCast(i));
            }
        }

        fn renderInst(self: *Self, i: u32) !void {
            const code = self.bc.code;
            const writer = self.stream.writer();

            const tag = code.items(.tag)[i];
            const payload = code.items(.payload)[i];
            const dst: u32 = @intFromEnum(payload.dst);

            try writer.print("{s} ", .{@tagName(tag)});
            switch (tag) {
                .ld => {
                    var int: u32 = undefined;
                    var float: f32 = undefined;
                    @memcpy(asBytes(&int), &payload.ops.imm);
                    @memcpy(asBytes(&float), &payload.ops.imm);

                    try writer.print("x{}, 0x{x:0>8} ({})\n", .{ dst, int, float });
                },
                .ldw => {
                    var int: u64 = undefined;
                    var float: f64 = undefined;
                    @memcpy(asBytes(&int), &payload.ops.wimm);
                    @memcpy(asBytes(&float), &payload.ops.wimm);

                    try writer.print("x{}, 0x{x:0>16} ({})\n", .{ dst, int, float });
                },
                .ineg,
                .fneg,
                .binv,
                .lnot,
                .mov,
                .itof,
                .ftoi,
                => {
                    const op: u32 = @intFromEnum(payload.ops.unary);
                    try writer.print("x{}, x{}\n", .{ dst, op });
                },
                .branch => {
                    const condition: u32 = @intFromEnum(payload.ops.branch.condition);
                    const target = payload.ops.branch.target;
                    const offset = @as(i64, target) - i;
                    try writer.print("x{}, {} ({})\n", .{ condition, target, offset });
                },
                .jump => {
                    const target = payload.ops.target;
                    const offset = @as(i64, target) - i;
                    try writer.print("{} ({})\n", .{ target, offset });
                },
                .exit => try writer.print("\n", .{}),
                else => {
                    const op1: u32 = @intFromEnum(payload.ops.binary.op1);
                    const op2: u32 = @intFromEnum(payload.ops.binary.op2);
                    try writer.print("x{}, x{}, x{}\n", .{ dst, op1, op2 });
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
