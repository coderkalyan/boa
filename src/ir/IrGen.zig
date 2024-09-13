const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Ir = @import("Ir.zig");
const Scope = @import("Scope.zig");
const Liveness = @import("Liveness.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const Inst = Ir.Inst;

const IrGen = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
tree: *const Ast,
insts: Ir.List,
extra: std.ArrayListUnmanaged(u32),
blocks: std.ArrayListUnmanaged(Ir.Block),
scratch: std.ArrayListUnmanaged(u32),
current_block: std.ArrayListUnmanaged(Ir.Index),
// current_block: Scope.Block,

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

pub fn addSlice(ig: *IrGen, slice: []const u32) !Inst.ExtraSlice {
    const start: u32 = @intCast(ig.extra.items.len);
    try ig.extra.appendSlice(ig.gpa, slice);
    const end: u32 = @intCast(ig.extra.items.len);

    return Inst.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    };
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

pub fn addUnlinked(ig: *IrGen, inst: Ir.Inst) !Ir.Index {
    const len: u32 = @intCast(ig.insts.len);
    try ig.insts.append(ig.gpa, inst);
    return @enumFromInt(len);
}

pub inline fn linkInst(ig: *IrGen, index: Ir.Index) !void {
    try ig.current_block.append(ig.arena, index);
}

pub fn add(ig: *IrGen, inst: Ir.Inst) !Ir.Index {
    const index = try ig.addUnlinked(inst);
    try ig.linkInst(index);
    return index;
}

pub fn reserveUnlinked(ig: *IrGen, tag: Ir.Inst.Tag) !Ir.Index {
    const len: u32 = @intCast(ig.insts.len);
    try ig.insts.append(ig.gpa, .{ .tag = tag, .payload = undefined });
    return @enumFromInt(len);
}

pub fn reserve(ig: *IrGen, tag: Ir.Inst.Tag) !Ir.Index {
    const index = try ig.reserveUnlinked(tag);
    try ig.linkInst(index);
    return index;
}

pub fn update(ig: *IrGen, inst: Ir.Index, payload: Ir.Inst.Payload) void {
    ig.insts.items(.payload)[@intFromEnum(inst)] = payload;
}

pub fn addBlock(ig: *IrGen) !Ir.BlockIndex {
    const slice = try ig.addSlice(@ptrCast(ig.current_block.items));
    ig.current_block.clearRetainingCapacity();

    const index: u32 = @intCast(ig.blocks.items.len);
    try ig.blocks.append(ig.gpa, .{
        .insts = slice,
    });
    return @enumFromInt(index);
}

pub fn reserveBlock(ig: *IrGen) !Ir.Scope.BlockIndex {
    const index: u32 = @intCast(ig.blocks.items.len);
    try ig.blocks.append(ig.gpa, undefined);
    return @enumFromInt(index);
}

pub fn updateBlock(ig: *IrGen, index: Ir.Scope.BlockIndex) !Ir.Scope.BlockIndex {
    const slice = try ig.addSlice(@ptrCast(ig.current_block.items));
    ig.current_block.clearRetainingCapacity();
    ig.blocks.items[@intFromEnum(index)] = slice;
}

pub fn currentBlock(ig: *IrGen) Ir.BlockIndex {
    const index: u32 = @intCast(ig.blocks.items.len);
    return @enumFromInt(index);
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
        .blocks = .{},
        .scratch = .{},
        .current_block = .{},
        // .current_block = Scope.Block.init(&ig, &function.base),
    };

    var module: Scope.Module = .{};

    // try ig.declareSlots(&function, node);
    // for (0..function.var_slots.len) |i| {
    //     const slot = function.var_slots.get(i);
    //     std.debug.print("ty: {} live: {}\n", .{ slot.ty, slot.live });
    // }

    // const a = try pool.put(.{ .str = "a" });
    // const b = try pool.put(.{ .str = "b" });
    // std.debug.print("{?} {?}\n", .{ function.var_table.get(a), function.var_table.get(b) });

    // var toplevel_block = Scope.Block.init(&ig, &function.base);
    // var toplevel_block = Scope.Block.init(&ig, &function.base);
    // const entry = try ig.block(&module.base, node);
    try ig.block(&module.base, node);
    _ = try ig.addBlock();
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
        .blocks = try ig.blocks.toOwnedSlice(gpa),
        .liveness = undefined,
    };
    ir = ir;

    const liveness = try Liveness.analyze(gpa, &ir);
    ir.liveness = liveness;
    return ir;
}

// fn block(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Scope.BlockIndex {
fn block(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!void {
    var inner = Scope.Block.init(ig, scope);
    defer inner.deinit();

    try ig.blockInner(&inner.base, node);
    // return ig.addScope.Block(&inner);
}

fn blockInner(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!void {
    const data = ig.tree.data(node).block;
    const sl = ig.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = ig.tree.extraSlice(sl);

    for (stmts) |stmt| {
        _ = try ig.statement(scope, stmt);
    }
}

fn blockLocals(
    ig: *IrGen,
    scope: *Scope,
    node: Node.Index,
    locals: *std.AutoHashMapUnmanaged(InternPool.Index, void),
) !void {
    _ = scope;
    const data = ig.tree.data(node).block;
    const sl = ig.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = ig.tree.extraSlice(sl);

    for (stmts) |stmt| {
        switch (ig.tree.data(stmt)) {
            .assign_simple => {
                const assign = ig.tree.data(stmt).assign_simple;
                const ident_token = ig.tree.mainToken(assign.ptr);
                const ident_str = ig.tree.tokenString(ident_token);
                const id = try ig.pool.put(.{ .str = ident_str });
                try locals.put(ig.arena, id, {});
            },
            else => {},
        }
    }
}

fn statement(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const tag = ig.tree.data(node);
    return switch (tag) {
        .assign_simple => ig.assignSimple(scope, node),
        .if_else => ig.ifElse(scope, node),
        .while_loop => ig.whileLoop(scope, node),
        .return_val => ig.returnVal(scope, node),
        else => {
            std.debug.print("unimplemented tag: {}\n", .{tag});
            unreachable;
        },
    };
}

fn assignSimple(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const assign = ig.tree.data(node).assign_simple;
    const ident_token = ig.tree.mainToken(assign.ptr);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    const val = try ig.valExpr(scope, assign.val);
    const b = scope.cast(Scope.Block).?;
    try b.vars.put(ig.arena, id, val);
    return val;
}

fn unionLocals(
    arena: Allocator,
    blocks: []const *const Scope.Block,
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

fn ifElse(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const b = scope.cast(Scope.Block).?;
    const if_else = ig.tree.data(node).if_else;
    const exec = ig.tree.extraData(if_else.exec, Node.IfElse);

    // the condition can be evaluated in the "parent" scope
    const cond = try ig.valExpr(scope, if_else.condition);
    const br = try ig.reserve(.br);
    const entry_block = try ig.addBlock();

    var inner_if = Scope.Block.init(ig, scope);
    defer inner_if.deinit();
    try ig.blockInner(&inner_if.base, exec.exec_true);
    const jmp1 = try ig.reserve(.jmp);
    const exec_if = try ig.addBlock();

    var inner_else = Scope.Block.init(ig, scope);
    defer inner_else.deinit();
    try ig.blockInner(&inner_else.base, exec.exec_true);
    const jmp2 = try ig.reserve(.jmp);
    const exec_else = try ig.addBlock();

    ig.update(jmp1, .{ .block = ig.currentBlock() });
    ig.update(jmp2, .{ .block = ig.currentBlock() });

    const branch = try ig.addExtra(Inst.Branch{
        .exec_if = exec_if,
        .exec_else = exec_else,
    });
    ig.update(br, .{ .unary_extra = .{ .op = cond, .extra = branch } });

    var union_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try unionLocals(ig.arena, &.{ b, &inner_if, &inner_else }, &.{}, &union_locals);
    var it = union_locals.iterator();
    var phi_index: u32 = 0;
    while (it.next()) |entry| : (phi_index += 1) {
        const ident = entry.key_ptr.*;
        const live_parent = b.vars.contains(ident);
        const live_if = inner_if.vars.contains(ident);
        const live_else = inner_else.vars.contains(ident);

        // if variable not mutated inside the if statement, nothing to merge
        if (!live_if and !live_else) continue;

        const phi_data: Ir.Inst.Phi = if (live_if) phi: {
            // three posibilities to merge:
            // 1) defined in if and else -> merge two children (overwrites parent)
            // 2) defined in if and parent -> merge child and parent
            // 3) defined only in if -> merge with undef
            if (live_else) {
                break :phi .{
                    .ty = ig.getTempIr().typeOf(inner_if.vars.get(ident).?),
                    .src1 = inner_if.vars.get(ident).?,
                    .block1 = exec_if,
                    .src2 = inner_else.vars.get(ident).?,
                    .block2 = exec_else,
                };
            } else if (live_parent) {
                break :phi .{
                    .ty = ig.getTempIr().typeOf(b.vars.get(ident).?),
                    .src1 = b.vars.get(ident).?,
                    .block1 = entry_block,
                    .src2 = inner_if.vars.get(ident).?,
                    .block2 = exec_if,
                };
            } else unreachable; // TODO: implement this
        } else phi: {
            // two posibilities to merge:
            // 1) defined in else and parent -> merge child and parent
            // 2) defined only in else -> merge with undef
            if (live_parent) {
                break :phi .{
                    .ty = ig.getTempIr().typeOf(b.vars.get(ident).?),
                    .src1 = b.vars.get(ident).?,
                    .block1 = entry_block,
                    .src2 = inner_else.vars.get(ident).?,
                    .block2 = exec_else,
                };
            } else unreachable; // TODO: implement this
        };

        const phi_extra = try ig.addExtra(phi_data);
        const phi = try ig.add(.{ .tag = .phi, .payload = .{ .extra = phi_extra } });
        try b.vars.put(b.ig.arena, ident, phi);
    }

    return br;
}

fn blockLoop(
    b: *Scope.Block,
    scope: *Scope,
    node: Node.Index,
) error{OutOfMemory}!Ir.ExtraIndex {
    var inner = Scope.Block.init(b.ig, scope);
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
    return b.addScope.Block(&inner);
}

fn whileLoop(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const b = scope.cast(Scope.Block).?;
    const while_loop = ig.tree.data(node).while_loop;

    // loops have multiple types of phis:
    // 1) top of body: between entry and bottom of loop body
    // 2) exit: between entry and bottom of loop body
    // more for control flow change (break, continue)
    // consider all locals defined anywhere, with a pre-pass over the loop body
    // so we can use the phi assignment in the loop body generation below
    var body_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try ig.blockLocals(scope, while_loop.body, &body_locals);
    var union_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try unionLocals(ig.arena, &.{b}, &.{&body_locals}, &union_locals);

    // prune to only the ones that are mutated in the loop body and need merging
    var mutated_locals: std.ArrayListUnmanaged(InternPool.Index) = .{};
    try mutated_locals.ensureTotalCapacity(ig.arena, body_locals.count());
    var it = union_locals.iterator();
    while (it.next()) |entry| {
        const ident = entry.key_ptr.*;
        if (body_locals.contains(ident)) mutated_locals.appendAssumeCapacity(ident);
    }

    // create the block contexts for body and condition so we can reserve phis
    // for our locals
    var inner_body = Scope.Block.init(ig, scope);
    defer inner_body.deinit();
    var inner_condition = Scope.Block.init(ig, scope);
    defer inner_condition.deinit();

    // jump from entry to condition
    const jmp1 = try ig.reserve(.jmp);
    const entry_block = try ig.addBlock();

    // reserve phis for each mutated local
    const scratch_top = ig.scratch.items.len;
    defer ig.scratch.shrinkRetainingCapacity(scratch_top);
    try ig.scratch.ensureUnusedCapacity(ig.arena, mutated_locals.items.len);
    for (mutated_locals.items) |ident| {
        const src_entry = b.vars.get(ident).?;
        const phi_data = try ig.addExtra(Inst.Phi{
            .ty = ig.getTempIr().typeOf(src_entry),
            .src1 = src_entry,
            .block1 = entry_block,
            .src2 = undefined,
            .block2 = undefined,
        });
        const phi = try ig.addUnlinked(.{ .tag = .phi, .payload = .{ .extra = phi_data } });
        ig.scratch.appendAssumeCapacity(@intFromEnum(phi));
        try inner_condition.vars.put(ig.arena, ident, phi);
        try inner_body.vars.put(ig.arena, ident, phi);
        try b.vars.put(ig.arena, ident, phi);
    }

    // generate the body
    try ig.blockInner(&inner_body.base, while_loop.body);
    const jmp2 = try ig.reserve(.jmp);
    const body = try ig.addBlock();

    // generate the condition, inside its own block since its an arbitrarily complex
    // expression that needs to execute every loop iteration (not just once like if/else)
    // the phis are actually placed in the condition block
    const phis = ig.scratch.items[scratch_top..];
    for (phis) |phi| try ig.linkInst(@enumFromInt(phi));
    const condition = try ig.valExpr(&inner_condition.base, while_loop.condition);
    const br = try ig.reserve(.br);
    const condition_block = try ig.addBlock();

    // update the condition/body phis to point to the vars that we know now
    for (mutated_locals.items, phis) |ident, phi| {
        const phi_data = ig.getTempIr().instPayload(@enumFromInt(phi)).extra;
        // TODO: cleaner way to patch this
        const src_body = @intFromEnum(inner_body.vars.get(ident).?);
        ig.extra.items[@intFromEnum(phi_data) + 3] = src_body;
        ig.extra.items[@intFromEnum(phi_data) + 4] = @intFromEnum(body);
    }

    const exit = ig.currentBlock();
    ig.update(jmp1, .{ .block = condition_block });
    ig.update(jmp2, .{ .block = condition_block });
    const branch = try ig.addExtra(Inst.Branch{
        .exec_if = body,
        .exec_else = exit,
    });
    ig.update(br, .{ .unary_extra = .{ .op = condition, .extra = branch } });

    return br;
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

inline fn valExpr(ig: *IrGen, s: *Scope, node: Node.Index) !Ir.Index {
    const ri: ResultInfo = .{ .semantics = .val };
    return ig.expr(s, ri, node);
}

inline fn ptrExpr(b: *Scope.Block, s: *Scope, node: Node.Index) !Ir.Index {
    const ri: ResultInfo = .{ .semantics = .ptr };
    return expr(b, s, ri, node);
}

fn expr(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    return switch (ri.semantics) {
        .val => switch (ig.tree.data(node)) {
            .bool_literal => ig.boolLiteral(scope, node),
            .integer_literal => ig.integerLiteral(scope, node),
            .float_literal => ig.floatLiteral(scope, node),
            .ident => ig.identExpr(scope, ri, node),
            .unary => ig.unaryExpr(scope, node),
            .binary => ig.binaryExpr(scope, node),
            else => ig.unexpectedNode(node),
        },
        .ptr => switch (ig.tree.data(node)) {
            // .ident => identExpr(b, scope, ri, node),
            else => ig.unexpectedNode(node),
        },
    };
}

fn boolLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const bool_token = ig.tree.mainToken(node);

    return switch (ig.tree.tokenTag(bool_token)) {
        .k_true => ig.add(.{ .tag = .constant, .payload = .{ .ip = .true } }),
        .k_false => ig.add(.{ .tag = .constant, .payload = .{ .ip = .false } }),
        else => ig.unexpectedNode(node),
    };
}

fn integerLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const integer_token = ig.tree.mainToken(node);
    const integer_str = ig.tree.tokenString(integer_token);

    const val = parseIntegerLiteral(integer_str);
    const ip = try ig.pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = val } } });
    return ig.add(.{ .tag = .constant, .payload = .{ .ip = ip } });
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

fn floatLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const float_token = ig.tree.mainToken(node);
    _ = float_token;

    const ip = try ig.pool.put(.{ .tv = .{ .ty = .float, .val = .{ .float = 1.0 } } });
    return ig.add(.{ .tag = .constant, .payload = .{ .ip = ip } });
}

fn identExpr(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    const ident_token = ig.tree.mainToken(node);
    const ident_str = ig.tree.tokenString(ident_token);
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

fn binaryExpr(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Index {
    const op_token = ig.tree.mainToken(node);
    const binary = ig.tree.data(node).binary;
    const token_tag = ig.tree.tokenTag(op_token);

    var l = try ig.valExpr(scope, binary.left);
    var r = try ig.valExpr(scope, binary.right);
    if (token_tag == .slash) {
        if (ig.typeOf(l) == .int) l = try ig.add(.{
            .tag = .itof,
            .payload = .{ .unary = l },
        });

        if (ig.typeOf(r) == .int) r = try ig.add(.{
            .tag = .itof,
            .payload = .{ .unary = r },
        });
    } else {
        try ig.binaryFloatDecay(&l, &r);
    }

    const lty = ig.typeOf(l);
    const rty = ig.typeOf(r);
    std.debug.assert(lty == rty);
    // const ty = lty;

    const tag: Ir.Inst.Tag = switch (ig.tree.tokenTag(op_token)) {
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

    return ig.add(.{
        .tag = tag,
        .payload = .{ .binary = .{ .l = l, .r = r } },
    });
}

fn unaryExpr(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Index {
    const op_token = ig.tree.mainToken(node);
    const unary = ig.tree.data(node).unary;

    const operand = try ig.valExpr(scope, unary);
    const tag: Ir.Inst.Tag = switch (ig.tree.tokenTag(op_token)) {
        .plus => unreachable, // TODO: implement this? or no-op
        .minus => .neg,
        .tilde => .binv,
        .k_not => .lnot,
        else => unreachable,
    };

    return ig.add(.{
        .tag = tag,
        .payload = .{ .unary = operand },
    });
}

fn binaryFloatDecay(ig: *IrGen, l: *Ir.Index, r: *Ir.Index) !void {
    const lty = ig.typeOf(l.*);
    const rty = ig.typeOf(r.*);

    switch (lty) {
        .int => switch (rty) {
            // nop
            .int => {},
            // decay left to float
            .float => l.* = try ig.add(.{
                .tag = .itof,
                .payload = .{ .unary = l.* },
            }),
            else => unreachable,
        },
        .float => switch (rty) {
            // decay right to float
            .int => r.* = try ig.add(.{
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

fn returnVal(ig: *IrGen, scope: *Scope, node: Node.Index) error{OutOfMemory}!Ir.Index {
    const return_val = ig.tree.data(node).return_val;
    const operand = try ig.valExpr(scope, return_val);
    return ig.add(.{
        .tag = .ret,
        .payload = .{ .unary = operand },
    });
}

fn getTempIr(ig: *const IrGen) Ir {
    return .{
        .pool = ig.pool,
        .tree = ig.tree,
        .insts = ig.insts.slice(),
        .extra = ig.extra.items,
        .blocks = ig.blocks.items,
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
fn unexpectedNode(ig: *IrGen, node: Node.Index) noreturn {
    const data = ig.tree.nodes.items(.data)[node];
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
