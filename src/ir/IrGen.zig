const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Ir = @import("Ir.zig");
const Scope = @import("Scope.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const Inst = Ir.Inst;
const Block = Scope.Block;

const IrGen = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
tree: *const Ast,
insts: Ir.List,
extra: std.ArrayListUnmanaged(u32),
scratch: std.ArrayListUnmanaged(u32),

pub fn addExtra(ig: *IrGen, extra: anytype) !Ir.ExtraIndex {
    const len: u32 = @intCast(ig.extra.items.len);
    const fields = std.meta.fields(@TypeOf(extra));
    try ig.extra.ensureUnusedCapacity(ig.gpa, fields.len);
    inline for (fields) |field| {
        switch (field.type) {
            inline else => {
                const num: u32 = @intFromEnum(@field(extra, field.name));
                ig.extra.appendAssumeCapacity(@bitCast(num));
            },
        }
    }
    return @enumFromInt(len);
}

pub fn extraData(ig: *IrGen, comptime T: type, index: Ir.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(ig.extra.items[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(ig: *IrGen, slice: Ir.Inst.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return ig.extra.items[start..end];
}

pub fn addSlice(ig: *IrGen, slice: []const u32) !Ir.ExtraIndex {
    const start: u32 = @intCast(ig.extra.items.len);
    try ig.extra.appendSlice(ig.gpa, slice);
    const end: u32 = @intCast(ig.extra.items.len);

    return ig.addExtra(Inst.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    });
}

// constructs a Ir.Inst by consolidating data insts and locs arrays
pub fn get(ig: *IrGen, index: Ir.Index) Ir.Inst {
    const i: u32 = @intFromEnum(index);
    std.debug.assert(i < ig.insts.len);
    return .{
        .data = ig.insts.get(i),
        .loc = ig.locs.items[i],
    };
}

pub fn add(ig: *IrGen, inst: Ir.Inst) !Ir.Index {
    const len: u32 = @intCast(ig.insts.len);
    try ig.insts.append(ig.gpa, inst);
    return @enumFromInt(len);
}

pub fn generate(gpa: Allocator, pool: *InternPool, tree: *const Ast, node: Node.Index) !Ir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var ig: IrGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .pool = pool,
        .tree = tree,
        .insts = .{},
        .extra = .{},
        .scratch = .{},
    };

    var module: Scope.Module = .{};
    var function = Scope.Function.init(&module.base);

    try ig.declareSlots(&function, node);
    for (0..function.var_slots.len) |i| {
        const slot = function.var_slots.get(i);
        std.debug.print("ty: {} live: {}\n", .{ slot.ty, slot.live });
    }

    const a = try pool.put(.{ .str = "a" });
    const b = try pool.put(.{ .str = "b" });
    std.debug.print("{?} {?}\n", .{ function.var_table.get(a), function.var_table.get(b) });

    // var toplevel_block = Block.init(&ig, &namespace.base);
    // _ = try block(&toplevel_block, &toplevel_block.base, node);
    // try blockInner()
    // post order format guarantees that the module node will be the last
    // const module_node: u32 = @intCast(tree.nodes.len - 1);
    // const module_slice = tree.extraData(tree.data(module_node).module.stmts, Node.ExtraSlice);
    // const module_stmts = tree.extraSlice(module_slice);
    // for (module_stmts) |stmt| {
    //     if (@as(Node.Data.Tag, tree.data(stmt)) == .function) {
    //         ig.lowerFunction(stmt);
    //     }
    // }
    // for (module)

    return .{
        .pool = pool,
        .tree = tree,
        .insts = ig.insts.toOwnedSlice(),
        .extra = try ig.extra.toOwnedSlice(gpa),
    };
}

fn declareSlots(ig: *IrGen, func: *Scope.Function, node: Node.Index) !void {
    const tree = ig.tree;
    const data = tree.data(node).block;
    const sl = tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = tree.extraSlice(sl);

    for (stmts) |stmt| {
        switch (tree.data(stmt)) {
            .assign_simple => |assign| switch (tree.data(assign.ptr)) {
                .ident => {
                    const ident_token = tree.mainToken(assign.ptr);
                    const ident_str = tree.tokenString(ident_token);
                    const ident = try ig.pool.put(.{ .str = ident_str });
                    _ = try func.reserveSlot(ig.arena, ident);
                },
                else => unreachable, // TODO: unimplemented
            },
            .if_simple => |if_simple| try ig.declareSlots(func, if_simple.exec_true),
            // TODO: if_else, if_chain
            .for_loop => |for_loop| try ig.declareSlots(func, for_loop.body),
            .while_loop => |while_loop| try ig.declareSlots(func, while_loop.body),
            else => {},
        }
    }
}

fn block(b: *Block, scope: *Scope, node: Node.Index) !Ir.ExtraIndex {
    var inner = Block.init(b.ig, scope);
    defer inner.deinit();

    try blockInner(&inner, &inner.base, node);
    return b.addBlock(&inner);
}

fn blockInner(b: *Block, scope: *Scope, node: Node.Index) !void {
    const data = b.tree.data(node).block;

    const sl = b.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = b.tree.extraSlice(sl);

    for (stmts) |stmt| {
        _ = try statement(b, scope, stmt);
    }
}

fn statement(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const tag = b.tree.data(node);

    return switch (tag) {
        .assign_simple => assignSimple(b, scope, node),
        else => {
            std.debug.print("unimplemented tag: {}\n", .{tag});
            unreachable;
        },
    };
}

fn assignSimple(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const assign = b.tree.data(node).assign_simple;

    const val = try valExpr(b, scope, assign.val);
    return val; // TODO: this is completely wrong
}

const ResultInfo = struct {
    semantics: Semantics,

    const Semantics = enum {
        // the expression should generate a value that can be used, loading from memory
        // if needed
        val,
        // the expression should generate a target (lvalue) that can be stored to
        ptr,
    };
};

inline fn valExpr(b: *Block, s: *Scope, node: Node.Index) !Ir.Index {
    const ri: ResultInfo = .{ .semantics = .val };
    return expr(b, s, ri, node);
}

inline fn ptrExpr(b: *Block, s: *Scope, node: Node.Index) !Ir.Index {
    const ri: ResultInfo = .{ .semantics = .ptr };
    return expr(b, s, ri, node);
}

fn expr(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    return switch (ri.semantics) {
        .val => switch (b.tree.data(node)) {
            .bool_literal => boolLiteral(b, scope, node),
            else => @enumFromInt(0),
        },
        .ptr => unreachable,
    };
}

fn boolLiteral(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;

    return switch (b.tree.tokenTag(node)) {
        .k_true => b.add(.{ .tag = .constant, .payload = .{ .ip = .true } }),
        .k_false => b.add(.{ .tag = .constant, .payload = .{ .ip = .false } }),
        else => unexpectedNode(b, node),
    };
}

fn unexpectedNode(b: *Block, node: Node.Index) noreturn {
    std.debug.print("encountered unexpected node: {}\n", .{b.tree.tokenTag(node)});
    unreachable;
}
