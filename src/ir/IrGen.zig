const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Ir = @import("Ir.zig");
const Scope = @import("Scope.zig");
const Liveness = @import("Liveness.zig");

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

pub fn reserve(ig: *IrGen, tag: Ir.Inst.Tag) !Ir.Index {
    const len: u32 = @intCast(ig.insts.len);
    try ig.insts.append(ig.gpa, .{ .tag = tag, .payload = undefined });
    return @enumFromInt(len);
}

pub fn update(ig: *IrGen, inst: Ir.Index, payload: Ir.Inst.Payload) void {
    ig.insts.items(.payload)[@intFromEnum(inst)] = payload;
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

    // try ig.declareSlots(&function, node);
    // for (0..function.var_slots.len) |i| {
    //     const slot = function.var_slots.get(i);
    //     std.debug.print("ty: {} live: {}\n", .{ slot.ty, slot.live });
    // }

    // const a = try pool.put(.{ .str = "a" });
    // const b = try pool.put(.{ .str = "b" });
    // std.debug.print("{?} {?}\n", .{ function.var_table.get(a), function.var_table.get(b) });

    var toplevel_block = Block.init(&ig, &function.base);
    const block_index = try block(&toplevel_block, &toplevel_block.base, node);
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
    var ir: Ir = .{
        .pool = pool,
        .tree = tree,
        .insts = ig.insts.toOwnedSlice(),
        .extra = try ig.extra.toOwnedSlice(gpa),
        .block = block_index,
        .liveness = undefined,
    };

    const liveness = try Liveness.analyze(gpa, &ir);
    ir.liveness = liveness;
    return ir;
}

// fn declareSlots(ig: *IrGen, func: *Scope.Function, node: Node.Index) !void {
//     const tree = ig.tree;
//     const data = tree.data(node).block;
//     const sl = tree.extraData(data.stmts, Node.ExtraSlice);
//     const stmts = tree.extraSlice(sl);
//
//     for (stmts) |stmt| {
//         switch (tree.data(stmt)) {
//             .assign_simple => |assign| switch (tree.data(assign.ptr)) {
//                 .ident => {
//                     const ident_token = tree.mainToken(assign.ptr);
//                     const ident_str = tree.tokenString(ident_token);
//                     const ident = try ig.pool.put(.{ .str = ident_str });
//                     _ = try func.reserveSlot(ig.arena, ident);
//                 },
//                 else => unreachable, // TODO: unimplemented
//             },
//             .if_simple => |if_simple| try ig.declareSlots(func, if_simple.exec_true),
//             // TODO: if_else, if_chain
//             .for_loop => |for_loop| try ig.declareSlots(func, for_loop.body),
//             .while_loop => |while_loop| try ig.declareSlots(func, while_loop.body),
//             else => {},
//         }
//     }
// }

fn block(b: *Block, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.ExtraIndex {
    var inner = Block.init(b.ig, scope);
    defer inner.deinit();

    try blockInner(&inner, &inner.base, node);
    return b.addBlock(&inner);
}

fn blockInner(b: *Block, scope: *Scope, node: Node.Index) error{OutOfMemory}!void {
    const data = b.tree.data(node).block;

    const sl = b.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = b.tree.extraSlice(sl);

    for (stmts) |stmt| {
        _ = try statement(b, scope, stmt);
    }
}

fn blockLocals(
    b: *Block,
    scope: *Scope,
    node: Node.Index,
    locals: *std.AutoHashMapUnmanaged(InternPool.Index, void),
) !void {
    _ = scope;
    const data = b.tree.data(node).block;

    const sl = b.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = b.tree.extraSlice(sl);

    for (stmts) |stmt| {
        switch (b.tree.data(stmt)) {
            .assign_simple => {
                const assign = b.tree.data(stmt).assign_simple;
                const ident_token = b.tree.mainToken(assign.ptr);
                const ident_str = b.tree.tokenString(ident_token);
                const id = try b.ig.pool.put(.{ .str = ident_str });
                try locals.put(b.ig.arena, id, {});
            },
            else => {},
        }
    }
}

fn statement(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const tag = b.tree.data(node);

    return switch (tag) {
        .assign_simple => assignSimple(b, scope, node),
        .if_else => ifElse(b, scope, node),
        .while_loop => whileLoop(b, scope, node),
        else => {
            std.debug.print("unimplemented tag: {}\n", .{tag});
            unreachable;
        },
    };
}

fn assignSimple(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const assign = b.tree.data(node).assign_simple;
    const ident_token = b.tree.mainToken(assign.ptr);
    const ident_str = b.tree.tokenString(ident_token);
    const id = try b.ig.pool.put(.{ .str = ident_str });

    const val = try valExpr(b, scope, assign.val);
    try b.vars.put(b.ig.arena, id, val);
    return val;
}

fn unionLocals(
    arena: Allocator,
    blocks: []const *const Block,
    prepasses: []const *const std.AutoHashMapUnmanaged(InternPool.Index, void),
    locals: *std.AutoHashMapUnmanaged(InternPool.Index, void),
) !void {
    for (blocks) |b| {
        var iterator = b.vars.iterator();
        while (iterator.next()) |local| try locals.put(arena, local.key_ptr.*, {});
    }

    for (prepasses) |pass| {
        var iterator = pass.iterator();
        while (iterator.next()) |local| try locals.put(arena, local.key_ptr.*, {});
    }
}

fn ifElse(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const if_else = b.tree.data(node).if_else;
    const exec = b.tree.extraData(if_else.exec, Node.IfElse);

    // the condition can be evaluated in the "parent" scope
    const cond = try valExpr(b, scope, if_else.condition);

    var inner_true = Block.init(b.ig, scope);
    defer inner_true.deinit();
    try blockInner(&inner_true, &inner_true.base, exec.exec_true);
    const exec_true = try b.addBlock(&inner_true);

    var inner_false = Block.init(b.ig, scope);
    defer inner_false.deinit();
    try blockInner(&inner_false, &inner_false.base, exec.exec_false);
    const exec_false = try b.addBlock(&inner_false);

    const inst = try b.reserve(.if_else);
    // the majority of complexity here is generate phi annotations to
    // indicate that locals assigned in the if and/or else blocks should
    // be merged and exported to the local scope after the if statement

    const scratch_top = b.ig.scratch.items.len;
    defer b.ig.scratch.shrinkRetainingCapacity(scratch_top);

    // consider all locals defined anywhere
    var union_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try unionLocals(b.ig.arena, &.{ b, &inner_true, &inner_false }, &.{}, &union_locals);
    var it = union_locals.iterator();
    var phi_index: u32 = 0;
    while (it.next()) |entry| : (phi_index += 1) {
        const ident = entry.key_ptr.*;
        const live_parent = b.vars.contains(ident);
        const live_if = inner_true.vars.contains(ident);
        const live_else = inner_false.vars.contains(ident);

        // if variable not mutated inside the if statement, nothing to merge
        if (!live_if and !live_else) continue;

        const phi = if (live_if) phi: {
            // three posibilities to merge:
            // 1) defined in if and else -> merge two children (overwrites parent)
            // 2) defined in if and parent -> merge child and parent
            // 3) defined only in if -> merge with undef
            if (live_else) {
                break :phi try b.add(.{
                    .tag = .phi_if_else,
                    .payload = .{
                        .binary = .{
                            .l = inner_true.vars.get(ident).?,
                            .r = inner_false.vars.get(ident).?,
                        },
                    },
                });
            } else if (live_parent) {
                break :phi try b.add(.{
                    .tag = .phi_entry_if,
                    .payload = .{
                        .binary = .{
                            .l = b.vars.get(ident).?,
                            .r = inner_true.vars.get(ident).?,
                        },
                    },
                });
            } else unreachable; // TODO: implement this
        } else phi: {
            // two posibilities to merge:
            // 1) defined in else and parent -> merge child and parent
            // 2) defined only in else -> merge with undef
            if (live_parent) {
                break :phi try b.add(.{
                    .tag = .phi_entry_if,
                    .payload = .{
                        .binary = .{
                            .l = b.vars.get(ident).?,
                            .r = inner_false.vars.get(ident).?,
                        },
                    },
                });
            } else unreachable; // TODO: implement this
        };

        try b.ig.scratch.append(b.ig.arena, @intFromEnum(phi));
        try b.vars.put(b.ig.arena, ident, phi);
    }

    const phis = b.ig.scratch.items[scratch_top..];
    b.update(inst, .{
        .op_extra = .{
            .op = cond,
            .extra = try addExtra(b.ig, Inst.IfElse{
                .exec_true = exec_true,
                .exec_false = exec_false,
                .phis = try b.ig.addSlice(phis),
            }),
        },
    });

    return inst;
}

fn blockLoop(
    b: *Block,
    scope: *Scope,
    node: Node.Index,
) error{OutOfMemory}!Ir.ExtraIndex {
    var inner = Block.init(b.ig, scope);
    defer inner.deinit();

    // first, scan through the block and list the locals that are assigned inside
    const inner_locals: std.ArrayListUnmanaged(InternPool.Index) = .{};
    // try blockLocals(b, scope, node, &inner_locals);
    // and keep only the locals that already exist in the parent scope, since they
    // need phi logic
    var phi_locals: std.ArrayListUnmanaged(struct {
        id: InternPool.Index,
        parent_arg: Ir.Index,
        body_arg: Ir.Index,
        phi: Ir.Index,
    }) = .{};
    try phi_locals.ensureUnusedCapacity(b.ig.arena, inner_locals.items.len);
    for (inner_locals.items) |id| {
        if (b.vars.contains(id)) phi_locals.appendAssumeCapacity(.{
            .id = id,
            .parent_arg = undefined,
            .body_arg = undefined,
            .phi = undefined,
        });
    }

    // add phiargs for the parent outside the loop body
    for (phi_locals.items) |*local| {
        const parent_def = b.vars.get(local.id).?;
        local.parent_arg = try b.add(.{ .tag = .phi_if_else, .payload = .{ .unary = parent_def } });
    }
    // and phis at the top of the loop body (don't know the other arg yet)
    for (phi_locals.items) |*local| {
        local.phi = try inner.add(.{
            .tag = .phi,
            .payload = .{ .binary = .{ .l = local.parent_arg, .r = undefined } },
        });
    }
    // then, process the block like normal
    try blockInner(&inner, &inner.base, node);
    // finally, add phiargs for the loop body and update the phis
    for (phi_locals.items) |*local| {
        const local_def = inner.vars.get(local.id).?;
        local.body_arg = try inner.add(.{ .tag = .phiarg, .payload = .{ .unary = local_def } });
        b.ig.insts.items(.payload)[@intFromEnum(local.phi)].binary.r = local.body_arg;
    }
    // var iterator = inner_locals.iterator();
    // while (iterator.next()) |entry| {
    //     const id = entry.key_ptr.*;
    //     if (b.vars.get(id)) |parent_def| {
    //         const parent_arg = try b.add(.{ .tag = .phiarg, .payload = .{ .unary = parent_def } });
    //         _ = parent_arg;
    //     }
    // }

    // for if statements, add `phiarg` nodes for all local vars and pass
    // them to the parent scope
    // try export_vars.ensureUnusedCapacity(b.ig.arena, inner.vars.count());
    // var iterator = inner.vars.iterator();
    // while (iterator.next()) |entry| {
    //     const arg = try inner.add(.{ .tag = .phiarg, .payload = .{ .unary = entry.value_ptr.* } });
    //     export_vars.putAssumeCapacity(entry.key_ptr.*, arg);
    // }

    // now seal and return the block
    return b.addBlock(&inner);
}

fn whileLoop(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    const while_loop = b.tree.data(node).while_loop;

    // loops have multiple types of phis:
    // 1) top of body: between entry and bottom of loop body
    // 2) exit: between entry and bottom of loop body
    // more for control flow change (break, continue)
    // consider all locals defined anywhere, with a pre-pass over the loop body
    // so we can use the phi assignment in the loop body generation below
    var body_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try blockLocals(b, scope, while_loop.body, &body_locals);
    var union_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try unionLocals(b.ig.arena, &.{b}, &.{&body_locals}, &union_locals);

    const scratch_top = b.ig.scratch.items.len;
    defer b.ig.scratch.shrinkRetainingCapacity(scratch_top);

    var inner_phis = Block.init(b.ig, scope);
    defer inner_phis.deinit();
    var inner_condition = Block.init(b.ig, scope);
    defer inner_condition.deinit();
    var inner_body = Block.init(b.ig, scope);
    defer inner_body.deinit();

    // loop body generation
    // first, reserve the necessary phis at the top
    var it = union_locals.iterator();
    while (it.next()) |entry| {
        const ident = entry.key_ptr.*;
        // if variable not mutated inside the loop body, nothing to merge
        if (!body_locals.contains(ident)) continue;

        const body_phi = try inner_phis.add(.{
            .tag = .phi_entry_body_body,
            // todo: this isn't safe since we don't know the type of the second operand
            .payload = .{
                .binary = .{
                    .l = b.vars.get(ident).?,
                    .r = undefined,
                },
            },
        });
        try inner_phis.ig.scratch.append(b.ig.arena, @intFromEnum(body_phi));
        try inner_phis.vars.put(b.ig.arena, ident, body_phi);
        try inner_body.vars.put(b.ig.arena, ident, body_phi);
        try inner_condition.vars.put(b.ig.arena, ident, body_phi);
    }
    const phi_block = try b.addBlock(&inner_phis);

    // generate the condition, inside its own block since its an arbitrarily complex
    // expression that needs to execute every loop iteration (not just once like if/else)
    const condition = try valExpr(&inner_condition, &inner_condition.base, while_loop.condition);
    const condition_block = try b.addBlock(&inner_condition);

    // then generate the actual body code
    try blockInner(&inner_body, &inner_body.base, while_loop.body);
    const body = try b.addBlock(&inner_body);

    // and finally actually fill in the phis now that forward references
    // are resolved
    it = union_locals.iterator();
    var i: usize = 0;
    while (it.next()) |entry| {
        const ident = entry.key_ptr.*;
        // if variable not mutated inside the loop body, nothing to merge
        if (!body_locals.contains(ident)) continue;
        const body_phi = b.ig.scratch.items[scratch_top + i];
        inner_body.update(@enumFromInt(body_phi), .{
            .binary = .{
                .l = b.vars.get(ident).?,
                .r = inner_body.vars.get(ident).?,
            },
        });
        i += 1;
    }

    const loop = try b.reserve(.loop);

    // now, generate phis for the exit
    it = union_locals.iterator();
    while (it.next()) |entry| {
        const ident = entry.key_ptr.*;
        // if variable not mutated inside the loop body, nothing to merge
        if (!inner_body.vars.contains(ident)) continue;

        const exit_phi = try b.add(.{
            .tag = .phi_entry_body_exit,
            .payload = .{
                .binary = .{
                    .l = b.vars.get(ident).?,
                    .r = inner_body.vars.get(ident).?,
                },
            },
        });
        try b.ig.scratch.append(b.ig.arena, @intFromEnum(exit_phi));
        try b.vars.put(b.ig.arena, ident, exit_phi);
    }

    const phis = b.ig.scratch.items[scratch_top..];
    b.update(loop, .{
        .op_extra = .{
            .op = condition,
            .extra = try addExtra(b.ig, Inst.Loop{
                .condition = condition_block,
                .body = body,
                .phi_block = phi_block,
                .phis = try b.ig.addSlice(phis),
            }),
        },
    });

    return loop;
}

const ResultInfo = struct {
    semantics: Semantics,
    type_hint: ?InternPool.Index = null,

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
            .integer_literal => integerLiteral(b, scope, node),
            .float_literal => floatLiteral(b, scope, node),
            .ident => identExpr(b, scope, ri, node),
            .unary => unaryExpr(b, scope, node),
            .binary => binaryExpr(b, scope, node),
            else => unexpectedNode(b, node),
        },
        .ptr => switch (b.tree.data(node)) {
            .ident => identExpr(b, scope, ri, node),
            else => unexpectedNode(b, node),
        },
    };
}

fn boolLiteral(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const bool_token = b.tree.mainToken(node);

    return switch (b.tree.tokenTag(bool_token)) {
        .k_true => b.add(.{ .tag = .constant, .payload = .{ .ip = .true } }),
        .k_false => b.add(.{ .tag = .constant, .payload = .{ .ip = .false } }),
        else => unexpectedNode(b, node),
    };
}

fn integerLiteral(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const integer_token = b.tree.mainToken(node);
    const integer_str = b.tree.tokenString(integer_token);

    const val = parseIntegerLiteral(integer_str);
    const ip = try b.ig.pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = val } } });
    return b.add(.{ .tag = .constant, .payload = .{ .ip = ip } });
}

pub fn parseIntegerLiteral(source: []const u8) u64 {
    const State = enum {
        start,
        radix,
        bin,
        oct,
        dec,
        hex,
    };

    var state: State = .start;
    var val: u64 = 0;
    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];
        switch (state) {
            .start => switch (c) {
                '1'...'9' => {
                    state = .dec;
                    val = c - '0';
                },
                '0' => state = .radix,
                else => unreachable,
            },
            .radix => switch (c) {
                // zero leading decimals are illegal, except for underscore separated
                // "zero" literals, where we have no more work to do
                '0', '_' => break,
                'b', 'B' => state = .bin,
                'o', 'O' => state = .oct,
                'x', 'X' => state = .hex,
                else => unreachable,
            },
            .bin => switch (c) {
                '_' => {},
                '0'...'1' => val = (val * 2) + (c - '0'),
                else => unreachable,
            },
            .oct => switch (c) {
                '_' => {},
                '0'...'7' => val = (val * 8) + (c - '0'),
                else => unreachable,
            },
            .dec => switch (c) {
                '_' => {},
                '0'...'9' => val = (val * 10) + (c - '0'),
                else => unreachable,
            },
            .hex => switch (c) {
                '_' => {},
                '0'...'9' => val = (val * 16) + (c - '0'),
                'a'...'f' => val = (val * 16) + (c - 'a' + 0xa),
                'A'...'F' => val = (val * 16) + (c - 'A' + 0xa),
                else => unreachable,
            },
        }
    }

    return val;
}

fn floatLiteral(b: *Block, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const float_token = b.tree.mainToken(node);
    _ = float_token;

    const ip = try b.ig.pool.put(.{ .tv = .{ .ty = .float, .val = .{ .float = 1.0 } } });
    return b.add(.{ .tag = .constant, .payload = .{ .ip = ip } });
}

fn identExpr(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    const ig = b.ig;
    const ident_token = b.tree.mainToken(node);
    const ident_str = b.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    // TODO: currently, we only support function scope (not modules)
    // const func = scope.declScope().cast(Scope.Function).?;
    switch (ri.semantics) {
        // since we're loading the variable to get a value, the identifier
        // *must* exist - else error
        // all variables are stack allocated from the IR's point of view, so
        // insert a "load" to return the value of the variable
        .val => {
            const ident_scope = scope.resolveIdent(id) orelse {
                std.debug.print("unknown identifier: {s}\n", .{ident_str});
                unreachable;
            };

            std.debug.assert(ident_scope.tag == .block);
            // TODO: check if the type is undef-union and insert a guard
            const ident_block = ident_scope.cast(Scope.Block).?;
            return ident_block.vars.get(id).?;
            // const alloc = ident_block.vars.get(id).?;
            // return b.add(.{
            //     .tag = .load,
            //     .payload = .{ .unary = alloc },
            // });
        },
        // since we're returning a variable to store to, if the identifer
        // doesn't exist we create (alloc) it
        // also, if the variable changed types, dealloc and realloc it
        .ptr => {
            // TODO: figure out the whole global business
            // if (b.vars.get(id)) |alloc| {
            //     _ = b.vars.remove(id);
            //     _ = try b.add(.{
            //         .tag = .dealloc,
            //         .payload = .{ .unary = alloc },
            //     });
            // }

            // const alloc = try b.add(.{
            //     .tag = .alloc,
            //     .payload = .{ .ip = ri.type_hint.? },
            // });
            // try b.vars.put(b.ig.arena, id, alloc);
            //
            // return alloc;
            unreachable;
        },
    }
}

fn binaryExpr(b: *Block, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Index {
    const ig = b.ig;
    const op_token = b.tree.mainToken(node);
    const binary = b.tree.data(node).binary;
    const token_tag = b.tree.tokenTag(op_token);

    var l = try valExpr(b, scope, binary.left);
    var r = try valExpr(b, scope, binary.right);
    if (token_tag == .slash) {
        if (ig.typeOf(l) == .int) l = try b.add(.{
            .tag = .itof,
            .payload = .{ .unary = l },
        });

        if (ig.typeOf(r) == .int) r = try b.add(.{
            .tag = .itof,
            .payload = .{ .unary = r },
        });
    } else {
        try binaryFloatDecay(b, &l, &r);
    }

    const lty = ig.typeOf(l);
    const rty = ig.typeOf(r);
    std.debug.assert(lty == rty);
    // const ty = lty;

    const tag: Ir.Inst.Tag = switch (b.tree.tokenTag(op_token)) {
        .plus => .add,
        .minus => .sub,
        .asterisk => .mul,
        .slash, .slash_slash => .div,
        .percent => .mod,
        .asterisk_asterisk => .pow,
        .equal_equal => .eq,
        .bang_equal => .ne,
        .l_angle => .lt,
        .r_angle => .gt,
        .l_angle_equal => .le,
        .r_angle_equal => .ge,
        .ampersand => .band,
        .pipe => .bor,
        .caret => .bxor,
        .l_angle_l_angle => .sll,
        .r_angle_r_angle => .sra,
        .k_or => .lor,
        .k_and => .land,
        else => unreachable,
    };

    return b.add(.{
        .tag = tag,
        .payload = .{ .binary = .{ .l = l, .r = r } },
    });
}

fn unaryExpr(b: *Block, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Index {
    const op_token = b.tree.mainToken(node);
    const unary = b.tree.data(node).unary;

    const operand = try valExpr(b, scope, unary);
    const tag: Ir.Inst.Tag = switch (b.tree.tokenTag(op_token)) {
        .plus => unreachable, // TODO: implement this? or no-op
        .minus => .neg,
        .tilde => .binv,
        .k_not => .lnot,
        else => unreachable,
    };

    return b.add(.{
        .tag = tag,
        .payload = .{ .unary = operand },
    });
}

fn binaryFloatDecay(b: *Block, l: *Ir.Index, r: *Ir.Index) !void {
    const ig = b.ig;
    const lty = ig.typeOf(l.*);
    const rty = ig.typeOf(r.*);

    switch (lty) {
        .int => switch (rty) {
            // nop
            .int => {},
            // decay left to float
            .float => l.* = try b.add(.{
                .tag = .itof,
                .payload = .{ .unary = l.* },
            }),
            else => unreachable,
        },
        .float => switch (rty) {
            // decay right to float
            .int => r.* = try b.add(.{
                .tag = .itof,
                .payload = .{ .unary = r.* },
            }),
            // nop
            .float => {},
            else => unreachable,
        },
        // nop for now, may need to revisit
        .bool => {},
        else => unreachable,
    }
}

fn getTempIr(ig: *const IrGen) Ir {
    return .{
        .pool = ig.pool,
        .tree = ig.tree,
        .insts = ig.insts.slice(),
        .extra = ig.extra.items,
        .block = undefined,
        .liveness = undefined,
    };
}

inline fn typeOf(ig: *const IrGen, inst: Ir.Index) InternPool.Index {
    return ig.getTempIr().typeOf(inst);
}

// in debug mode, this expands to a debug message with the unexpected node
// followed by a trap
// in release mode, it should optimize out entirely, and LLVM *should*
// propogate the unreachable into the corresponding clause in the caller's switch
// to optimize the jump table
fn unexpectedNode(b: *Block, node: Node.Index) noreturn {
    const data = b.tree.nodes.items(.data)[node];
    std.debug.print("encountered unexpected node: {}\n", .{data});
    unreachable;
}

test "parseIntegerLiteral" {
    try std.testing.expectEqual(0, parseIntegerLiteral("0"));
    try std.testing.expectEqual(1, parseIntegerLiteral("1"));
    try std.testing.expectEqual(9, parseIntegerLiteral("9"));
    try std.testing.expectEqual(123, parseIntegerLiteral("123"));
    try std.testing.expectEqual(123, parseIntegerLiteral("1_2_3"));
    try std.testing.expectEqual(456, parseIntegerLiteral("4___56_"));
    try std.testing.expectEqual(0, parseIntegerLiteral("0_0__000"));
}
