const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Ir = @import("Ir.zig");
const Scope = @import("Scope.zig");
const Liveness = @import("Liveness.zig");
const BlockBuilder = @import("BlockBuilder.zig");

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
builders: std.SegmentedList(BlockBuilder, 0),
free_builders: std.ArrayListUnmanaged(u32),
current_builder: *BlockBuilder,

pub fn addExtra(ig: *IrGen, extra: anytype) !Ir.ExtraIndex {
    const len: u32 = @intCast(ig.extra.items.len);
    const fields = std.meta.fields(@TypeOf(extra));
    try ig.extra.ensureUnusedCapacity(ig.gpa, fields.len);
    inline for (fields) |field| {
        switch (field.type) {
            inline else => {
                const num = @intFromEnum(@field(extra, field.name));
                ig.extra.appendAssumeCapacity(num);
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

fn createBlockAssumeCapacity(ig: *IrGen) *BlockBuilder {
    // allocate space in the sealed blocks list upfront so we know
    // what the final blockindex of the block will be
    const index: u32 = @intCast(ig.blocks.items.len);
    _ = ig.blocks.addOneAssumeCapacity();

    const slot = ig.free_builders.pop();
    const builder = ig.builders.at(slot);
    builder.* = BlockBuilder.init(ig, @enumFromInt(index), slot);
    return builder;
}

pub fn createBlock(ig: *IrGen) !*BlockBuilder {
    // if out of space, allocate a new builder
    if (ig.free_builders.items.len == 0) {
        const index: u32 = @intCast(ig.builders.count());
        _ = try ig.builders.addOne(ig.arena);
        try ig.free_builders.append(ig.arena, index);
    }

    // should definitely have a free builder now
    try ig.blocks.ensureUnusedCapacity(ig.gpa, 1);
    return ig.createBlockAssumeCapacity();
}

pub fn update(ig: *IrGen, inst: Ir.Index, payload: Ir.Inst.Payload) void {
    ig.insts.items(.payload)[@intFromEnum(inst)] = payload;
}

pub const Context = enum {
    module,
    function,
};

pub fn generate(
    comptime ctx: Context,
    gpa: Allocator,
    pool: *InternPool,
    tree: *const Ast,
    node: Node.Index,
    global: InternPool.Index,
) !Ir {
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
        .builders = .{},
        .free_builders = .{},
        .current_builder = undefined,
    };

    ig.current_builder = try ig.createBlock();
    switch (ctx) {
        .module => {
            var module: Scope.Module = .{ .context = .object_empty };
            try ig.moduleInner(&module.base, node);
        },
        .function => {
            // TODO: functions won't be able to see global scope
            std.debug.print("{}\n", .{pool.get(global)});
            var module: Scope.Module = .{ .context = global };
            var fs = Scope.Function.init(&module.base);
            try ig.functionInner(&fs.base, node);
        },
    }
    _ = try ig.current_builder.seal();

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

fn moduleInner(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!void {
    const data = ig.tree.data(node).module;
    const sl = ig.tree.extraData(Node.ExtraSlice, data.stmts);
    const stmts: []const Node.Index = @ptrCast(ig.tree.extraSlice(sl));

    for (stmts) |stmt| {
        try ig.statement(scope, stmt);
    }
}

fn functionInner(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!void {
    var inner = Scope.Block.init(ig, scope);
    const data = ig.tree.data(node).function;
    const signature = ig.tree.extraData(Node.FunctionSignature, data.signature);
    const slice = ig.tree.extraData(Node.ExtraSlice, signature.params);
    const params: []const Node.Index = @ptrCast(ig.tree.extraSlice(slice));
    for (params, 0..) |param, i| {
        const param_data = ig.tree.data(param).param;
        const param_token = ig.tree.mainToken(param);
        const param_str = ig.tree.tokenString(param_token);
        const id = try ig.pool.put(.{ .str = param_str });

        var ty: InternPool.Index = .any;
        if (param_data != .null) ty = try ig.typeExpr(scope, param_data);

        const arg = try ig.current_builder.arg(@intCast(i), ty);
        try inner.vars.put(ig.arena, id, arg);
    }

    try ig.block(&inner.base, data.body);
}

fn block(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!void {
    var inner = Scope.Block.init(ig, scope);
    try ig.blockInner(&inner.base, node);
}

fn blockInner(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!void {
    const data = ig.tree.data(node).block;
    const sl = ig.tree.extraData(Node.ExtraSlice, data.stmts);
    const stmts: []const Node.Index = @ptrCast(ig.tree.extraSlice(sl));

    for (stmts) |stmt| {
        try ig.statement(scope, stmt);
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
    const sl = ig.tree.extraData(Node.ExtraSlice, data.stmts);
    const stmts: []const Node.Index = @ptrCast(ig.tree.extraSlice(sl));

    for (stmts) |stmt| {
        switch (ig.tree.data(stmt)) {
            .assign_simple => {
                const assign = ig.tree.data(stmt).assign_simple;
                const ident_token = ig.tree.mainToken(assign.ptr);
                const ident_str = ig.tree.tokenString(ident_token);
                const id = try ig.pool.put(.{ .str = ident_str });
                try locals.put(ig.arena, id, {});
            },
            .assign_binary => {
                const assign = ig.tree.data(stmt).assign_binary;
                const ident_token = ig.tree.mainToken(assign.ptr);
                const ident_str = ig.tree.tokenString(ident_token);
                const id = try ig.pool.put(.{ .str = ident_str });
                try locals.put(ig.arena, id, {});
            },
            else => {},
        }
    }
}

fn statement(ig: *IrGen, scope: *Scope, node: Node.Index) !void {
    const tag = ig.tree.data(node);
    _ = switch (tag) {
        .assign_simple => try ig.assignSimple(scope, node),
        .assign_binary => try ig.assignBinary(scope, node),
        .if_simple => try ig.ifSimple(scope, node),
        .if_else => try ig.ifElse(scope, node),
        .while_loop => try ig.whileLoop(scope, node),
        .for_loop => try ig.forLoop(scope, node),
        .return_none => try ig.returnNone(scope, node),
        .return_val => try ig.returnVal(scope, node),
        .pass => {},
        .function => try ig.function(scope, node),
        .call => try ig.call(scope, node),
        else => ig.unexpectedNode(node),
    };
}

fn contextInsert(ig: *IrGen, ctx: *InternPool.Index, id: InternPool.Index, val: Ir.Index) !void {
    const ty = ig.getTempIr().typeOf(val);
    var object = ig.pool.get(ctx.*).ty;
    object = try object.objectInsert(
        ig.arena,
        .{ .name = id, .ty = ty },
    );
    ctx.* = try ig.pool.put(.{ .ty = object });
}

fn assignSimple(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const assign = ig.tree.data(node).assign_simple;
    const ident_token = ig.tree.mainToken(assign.ptr);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    // TODO: clean this up
    if (ig.tree.tag(assign.ptr) == .ident) {
        const val = try ig.valExpr(scope, assign.val);
        // how we treat the assignment depends on the context we're in -
        // module or function scope
        const context = scope.context();
        switch (context.tag) {
            // global variables are implemented by storing to the global "object",
            // which at runtime uses a hashtable wrapped by an inline cache
            .module => {
                const module = scope.cast(Scope.Module).?;
                try ig.contextInsert(&module.context, id, val);
                const object = ig.pool.get(module.context).ty;
                const slot = object.objectSlot(id).?;
                const global_ptr = try ig.current_builder.contextPtr(slot);
                return ig.current_builder.store(global_ptr, val);
            },
            // local variables are zero cost, we just map the identifier
            // to the expression value
            .function => {
                const b = scope.cast(Scope.Block).?;
                try b.vars.put(ig.arena, id, val);
                return val;
            },
            .block => unreachable,
        }
    } else unreachable;
}

fn assignBinary(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const assign = ig.tree.data(node).assign_binary;
    const ident_token = ig.tree.mainToken(assign.ptr);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    const op_token: Ast.TokenIndex = @enumFromInt(@intFromEnum(ident_token) + 1);
    const base = try ig.valExpr(scope, assign.ptr);
    const val = try ig.valExpr(scope, assign.val);
    const bin = try ig.binaryInner(op_token, base, val);
    // how we treat the assignment depends on the context we're in -
    // module or function scope
    const context = scope.context();
    switch (context.tag) {
        // global variables are implemented by storing to the global "object",
        // which at runtime uses a hashtable wrapped by an inline cache
        .module => {
            const module = scope.cast(Scope.Module).?;
            try ig.contextInsert(&module.context, id, val);
            const object = ig.pool.get(module.context).ty;
            const slot = object.objectSlot(id).?;
            const global_ptr = try ig.current_builder.contextPtr(slot);
            return ig.current_builder.store(global_ptr, val);
        },
        // local variables are zero cost, we just map the identifier
        // to the expression value
        .function => {
            const b = scope.cast(Scope.Block).?;
            try b.vars.put(ig.arena, id, bin);
            return val;
        },
        .block => unreachable,
    }
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

fn ifSimple(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const b = scope.cast(Scope.Block).?;
    const if_simple = ig.tree.data(node).if_simple;

    // we need at least two other blocks - for if and exit
    const if_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    // the condition can be evaluated in the entry block (current)
    const cond = try ig.valExpr(scope, if_simple.condition);
    const br = try ig.current_builder.br(cond, if_builder.index, exit_builder.index);
    const entry_block = try ig.current_builder.seal();

    ig.current_builder = if_builder;
    var inner_if = Scope.Block.init(ig, scope);
    try ig.blockInner(&inner_if.base, if_simple.exec_true);
    if (ig.getTempIr().instTag(ig.current_builder.last()) != .ret) {
        _ = try ig.current_builder.jmp(exit_builder.index);
    }
    const exec_if = try ig.current_builder.seal();

    ig.current_builder = exit_builder;
    try b.hoistMergeSingle(&inner_if, exec_if, entry_block);
    return br;
}

fn ifElse(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const b = scope.cast(Scope.Block).?;
    const if_else = ig.tree.data(node).if_else;
    const exec = ig.tree.extraData(Node.IfElse, if_else.exec);

    // we need at least three other blocks - for if, else, and exit
    const if_builder = try ig.createBlock();
    const else_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    // the condition can be evaluated in the entry block (current)
    const cond = try ig.valExpr(scope, if_else.condition);
    const br = try ig.current_builder.br(cond, if_builder.index, else_builder.index);
    const entry_block = try ig.current_builder.seal();

    ig.current_builder = if_builder;
    var inner_if = Scope.Block.init(ig, scope);
    try ig.blockInner(&inner_if.base, exec.exec_true);
    if (ig.getTempIr().instTag(ig.current_builder.last()) != .ret) {
        _ = try ig.current_builder.jmp(exit_builder.index);
    }
    const exec_if = try ig.current_builder.seal();

    ig.current_builder = else_builder;
    var inner_else = Scope.Block.init(ig, scope);
    try ig.blockInner(&inner_else.base, exec.exec_false);
    if (ig.getTempIr().instTag(ig.current_builder.last()) != .ret) {
        _ = try ig.current_builder.jmp(exit_builder.index);
    }
    const exec_else = try ig.current_builder.seal();

    ig.current_builder = exit_builder;
    try b.hoistMergeDouble(&inner_if, exec_if, &inner_else, exec_else, entry_block);
    return br;
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
    // TODO: should anything else be unioned here?
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
    var inner_condition = Scope.Block.init(ig, scope);

    // we need at least three other blocks - for condition, body, and exit
    const body_builder = try ig.createBlock();
    const condition_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    // jump from entry to condition
    _ = try ig.current_builder.jmp(condition_builder.index);
    const entry_block = try ig.current_builder.seal();

    // reserve phis for each mutated local in the condition
    const scratch_top = ig.scratch.items.len;
    defer ig.scratch.shrinkRetainingCapacity(scratch_top);
    try ig.scratch.ensureUnusedCapacity(ig.arena, mutated_locals.items.len);
    ig.current_builder = condition_builder;
    for (mutated_locals.items) |ident| {
        const src_entry = b.vars.get(ident).?;
        const phi = try ig.current_builder.phi(
            ig.getTempIr().typeOf(src_entry),
            src_entry,
            entry_block,
            undefined,
            undefined,
        );
        ig.scratch.appendAssumeCapacity(@intFromEnum(phi));
        try inner_condition.vars.put(ig.arena, ident, phi);
        try inner_body.vars.put(ig.arena, ident, phi);
        try b.vars.put(ig.arena, ident, phi);
    }

    // generate the body
    ig.current_builder = body_builder;
    const body_top = ig.current_builder.index;
    try ig.blockInner(&inner_body.base, while_loop.body);
    _ = try ig.current_builder.jmp(condition_builder.index);
    const body_bot = try ig.current_builder.seal();

    // generate the condition, inside its own block since its an arbitrarily complex
    // expression that needs to execute every loop iteration (not just once like if/else)
    // the phis are actually placed in the condition block
    const phis = ig.scratch.items[scratch_top..];
    ig.current_builder = condition_builder;
    const condition = try ig.valExpr(&inner_condition.base, while_loop.condition);
    const br = try ig.current_builder.br(condition, body_top, exit_builder.index);
    _ = try ig.current_builder.seal();

    // update the condition/body phis to point to the vars that we know now
    for (mutated_locals.items, phis) |ident, phi| {
        const phi_data = ig.getTempIr().instPayload(@enumFromInt(phi)).extra;
        // TODO: cleaner way to patch this
        const src_body = @intFromEnum(inner_body.vars.get(ident).?);
        ig.extra.items[@intFromEnum(phi_data) + 3] = src_body;
        ig.extra.items[@intFromEnum(phi_data) + 4] = @intFromEnum(body_bot);
    }

    ig.current_builder = exit_builder;
    return br;
}

const RangeIterable = struct {
    start: ?Node.Index,
    stop: Node.Index,
    step: ?Node.Index,
};

fn desugarRangeIterable(ig: *IrGen, iterable: Node.Index) !?RangeIterable {
    if (ig.tree.tag(iterable) != .call) return null;
    const call_data = ig.tree.data(iterable).call;
    const main_token = ig.tree.mainToken(call_data.ptr);
    const ident = ig.tree.tokenString(main_token);
    if (!std.mem.eql(u8, ident, "range")) return null;

    // at this point we know we have a range, just figure out how many
    // explicit args it has an fill in the implicit ones (start = 0, step = 1)
    const args_slice = ig.tree.extraData(Node.ExtraSlice, call_data.args);
    const args: []const Node.Index = @ptrCast(ig.tree.extraSlice(args_slice));
    return switch (args.len) {
        1 => .{ .start = null, .stop = args[0], .step = null },
        2 => .{ .start = args[0], .stop = args[1], .step = null },
        3 => .{ .start = args[0], .stop = args[1], .step = args[2] },
        else => null,
    };
}

fn forLoop(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const b = scope.cast(Scope.Block).?;
    const for_loop = ig.tree.data(node).for_loop;
    const signature = ig.tree.extraData(Node.ForSignature, for_loop.signature);
    const target_token = ig.tree.mainToken(signature.target);
    const target = try ig.pool.put(.{ .str = ig.tree.tokenString(target_token) });

    const range = (try ig.desugarRangeIterable(signature.iterable)) orelse {
        std.debug.print("currently only range based for loops are supported\n", .{});
        return ig.unexpectedNode(node);
    };

    // loops have multiple types of phis:
    // 1) top of body: between entry and bottom of loop body
    // 2) exit: between entry and bottom of loop body
    // more for control flow change (break, continue)
    // consider all locals defined anywhere, with a pre-pass over the loop body
    // so we can use the phi assignment in the loop body generation below
    var body_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try ig.blockLocals(scope, for_loop.body, &body_locals);
    var union_locals: std.AutoHashMapUnmanaged(InternPool.Index, void) = .{};
    try unionLocals(ig.arena, &.{b}, &.{&body_locals}, &union_locals);

    // prune to only the ones that are mutated in the loop body and need merging
    var mutated_locals: std.ArrayListUnmanaged(InternPool.Index) = .{};
    try mutated_locals.ensureTotalCapacity(ig.arena, body_locals.count() + 1);
    var it = union_locals.iterator();
    while (it.next()) |entry| {
        const ident = entry.key_ptr.*;
        if (body_locals.contains(ident)) mutated_locals.appendAssumeCapacity(ident);
    }
    mutated_locals.appendAssumeCapacity(target);

    // create the block contexts for body and condition so we can reserve phis
    // for our locals
    var inner_body = Scope.Block.init(ig, scope);
    var inner_condition = Scope.Block.init(ig, scope);

    // we need at least three other blocks - for condition, body, and exit
    const body_builder = try ig.createBlock();
    const condition_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    // initialize the target to a starting value
    const start = if (range.start) |s| inst: {
        break :inst try ig.valExpr(scope, s);
    } else try ig.current_builder.constant(.izero);
    try b.vars.put(ig.arena, target, start);
    defer _ = b.vars.remove(target); // TODO: this isn't really correct

    // jump from entry to condition
    _ = try ig.current_builder.jmp(condition_builder.index);
    const entry_block = try ig.current_builder.seal();

    // reserve phis for each mutated local
    const scratch_top = ig.scratch.items.len;
    defer ig.scratch.shrinkRetainingCapacity(scratch_top);
    try ig.scratch.ensureUnusedCapacity(ig.arena, mutated_locals.items.len);
    ig.current_builder = condition_builder;
    for (mutated_locals.items) |ident| {
        const src_entry = b.vars.get(ident).?;
        const phi = try ig.current_builder.phi(
            ig.getTempIr().typeOf(src_entry),
            src_entry,
            entry_block,
            undefined,
            undefined,
        );
        ig.scratch.appendAssumeCapacity(@intFromEnum(phi));
        try inner_condition.vars.put(ig.arena, ident, phi);
        try inner_body.vars.put(ig.arena, ident, phi);
        try b.vars.put(ig.arena, ident, phi);
    }

    // generate the body
    ig.current_builder = body_builder;
    const body_top = ig.current_builder.index;
    try ig.blockInner(&inner_body.base, for_loop.body);
    // and the afterthought (increment/decrement by range)
    const increment = if (range.step) |s| inst: {
        break :inst try ig.valExpr(scope, s);
    } else try ig.current_builder.constant(.ione);
    const afterthought = try ig.current_builder.binary(
        .add,
        inner_body.vars.get(target).?,
        increment,
    );
    try inner_body.vars.put(ig.arena, target, afterthought);
    _ = try ig.current_builder.jmp(condition_builder.index);
    const body_bot = try ig.current_builder.seal();

    // generate the condition, inside its own block since its an arbitrarily complex
    // expression that needs to execute every loop iteration (not just once like if/else)
    // the phis are actually placed in the condition block
    const phis = ig.scratch.items[scratch_top..];
    ig.current_builder = condition_builder;
    const stop = try ig.valExpr(scope, range.stop);
    const condition = try ig.current_builder.binary(.lt, b.vars.get(target).?, stop);
    const br = try ig.current_builder.br(condition, body_top, exit_builder.index);
    _ = try ig.current_builder.seal();

    // update the condition/body phis to point to the vars that we know now
    for (mutated_locals.items, phis) |ident, phi| {
        const phi_data = ig.getTempIr().instPayload(@enumFromInt(phi)).extra;
        // TODO: cleaner way to patch this
        const src_body = @intFromEnum(inner_body.vars.get(ident).?);
        ig.extra.items[@intFromEnum(phi_data) + 3] = src_body;
        ig.extra.items[@intFromEnum(phi_data) + 4] = @intFromEnum(body_bot);
    }

    ig.current_builder = exit_builder;
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
        // type,
    };
};

inline fn valExpr(ig: *IrGen, s: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const ri: ResultInfo = .{ .semantics = .val };
    return ig.expr(s, ri, node);
}

inline fn ptrExpr(b: *Scope.Block, s: *Scope, node: Node.Index) !Ir.Index {
    const ri: ResultInfo = .{ .semantics = .ptr };
    return expr(b, s, ri, node);
}

inline fn typeExpr(ig: *IrGen, s: *Scope, node: Node.Index) !InternPool.Index {
    _ = s;
    const ident_token = ig.tree.mainToken(node);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    return switch (id) {
        .builtin_int => .int,
        .builtin_float => .float,
        .builtin_bool => .bool,
        else => unreachable,
    };
    // const ri: ResultInfo = .{ .semantics = .type };
    // return ig.expr(s, ri, node);
}

fn expr(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    return switch (ri.semantics) {
        .val => switch (ig.tree.data(node)) {
            .none_literal => ig.noneLiteral(scope, node),
            .bool_literal => ig.boolLiteral(scope, node),
            .integer_literal => ig.integerLiteral(scope, node),
            .float_literal => ig.floatLiteral(scope, node),
            .string_literal => ig.stringLiteral(scope, node),
            .list_literal => ig.listLiteral(scope, node),
            .ident => ig.identExpr(scope, ri, node),
            .unary => ig.unaryExpr(scope, node),
            .binary => ig.binaryExpr(scope, node),
            .call => ig.call(scope, node),
            .subscript => ig.subscript(scope, ri, node),
            .attribute => ig.attribute(scope, ri, node),
            else => ig.unexpectedNode(node),
        },
        .ptr => switch (ig.tree.data(node)) {
            // .ident => identExpr(b, scope, ri, node),
            else => ig.unexpectedNode(node),
        },
        // .type => switch (ig.tree.data(node)) {
        //     .ident => ig.identExpr(scope, ri, node),
        //     else => ig.unexpectedNode(node),
        // },
    };
}

fn noneLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    _ = node;
    return ig.current_builder.constant(.none);
}

fn boolLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const bool_token = ig.tree.mainToken(node);
    return switch (ig.tree.tokenTag(bool_token)) {
        .k_true => ig.current_builder.constant(.true),
        .k_false => ig.current_builder.constant(.false),
        else => ig.unexpectedNode(node),
    };
}

fn integerLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const integer_token = ig.tree.mainToken(node);
    const integer_str = ig.tree.tokenString(integer_token);

    const val = parseIntegerLiteral(integer_str);
    const ip = try ig.pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = @intCast(val) } } });
    return ig.current_builder.constant(ip);
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
    return ig.current_builder.constant(ip);
}

fn stringLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    _ = scope;
    const token = ig.tree.mainToken(node);
    const token_str = ig.tree.tokenString(token);
    const literal = token_str[1 .. token_str.len - 1];
    const ip = try ig.pool.put(.{ .str = literal });

    return ig.current_builder.constant(ip);
}

fn listLiteral(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const list_literal = ig.tree.data(node).list_literal;
    const slice = ig.tree.extraData(Node.ExtraSlice, list_literal.elements);
    const ast_elements: []const Node.Index = @ptrCast(ig.tree.extraSlice(slice));

    const scratch_top = ig.scratch.items.len;
    defer ig.scratch.shrinkRetainingCapacity(scratch_top);
    try ig.scratch.ensureUnusedCapacity(ig.arena, ast_elements.len);
    for (ast_elements) |ast_element| {
        const element = try ig.valExpr(scope, ast_element);
        ig.scratch.appendAssumeCapacity(@intFromEnum(element));
    }

    const args = ig.scratch.items[scratch_top..];
    return ig.current_builder.listInit(@ptrCast(args));
}

fn identExpr(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    _ = ri;
    const ident_token = ig.tree.mainToken(node);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    // TODO: inline this with other uses
    // if (ri.semantics == .type) return ig.current_builder.constant(id);

    switch (id) {
        .builtin_print,
        .builtin_len,
        => return ig.current_builder.builtin(id),
        else => {},
    }

    // how we treat the assignment depends on the context we're in -
    // module or function scope
    const context = scope.context();
    switch (context.tag) {
        // global variables are implemented using a runtime hashtable lookup
        // (which will eventually be wrapped by an inline cache)
        .module => {
            const module = scope.cast(Scope.Module).?;
            const object = ig.pool.get(module.context).ty;
            const slot = object.objectSlot(id).?;
            const ty = object.object[slot].ty;
            const global_ptr = try ig.current_builder.contextPtr(slot);
            return ig.current_builder.load(global_ptr, ty);
        },
        // local variables are zero cost, we just map the identifier
        // to the expression value
        .function => {
            if (scope.resolveIdent(id)) |ident_scope| {
                std.debug.assert(ident_scope.tag == .block);
                // TODO: check if the type is undef-union and insert a guard
                const ident_block = ident_scope.cast(Scope.Block).?;
                return ident_block.vars.get(id).?;
            }

            const module = context.parent().?.cast(Scope.Module).?;
            const object = ig.pool.get(module.context).ty;
            const slot = object.objectSlot(id).?;
            const ty = object.object[slot].ty;
            const global_ptr = try ig.current_builder.contextPtr(slot);
            return ig.current_builder.load(global_ptr, ty);
        },
        .block => unreachable,
    }
}

fn binaryExpr(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const op_token = ig.tree.mainToken(node);
    const token_tag = ig.tree.tokenTag(op_token);
    // special evaluation order (short circuiting)
    if (token_tag == .k_or) return ig.logicalOr(scope, node);
    if (token_tag == .k_and) return ig.logicalAnd(scope, node);

    const binary = ig.tree.data(node).binary;
    const l = try ig.valExpr(scope, binary.left);
    const r = try ig.valExpr(scope, binary.right);
    return ig.binaryInner(op_token, l, r);
}

fn binaryInner(ig: *IrGen, op: Ast.TokenIndex, left: Ir.Index, right: Ir.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const token_tag = ig.tree.tokenTag(op);
    var l = left;
    var r = right;
    var lty = ig.typeOf(l);
    const rty = ig.typeOf(r);

    // special type handling - coerce one of the types to a float, the other
    // will follow suit below
    if (token_tag == .slash or token_tag == .slash_equal) {
        switch (lty) {
            .int => {
                l = try ig.current_builder.unary(.itof, l);
                lty = .float;
            },
            .bool => {
                l = try ig.current_builder.unary(.btoi, l);
                l = try ig.current_builder.unary(.itof, l);
            },
            else => {},
        }
    }

    // type coercion
    switch (lty) {
        .nonetype => return error.Unsupported,
        .int => switch (rty) {
            .nonetype => return error.Unsupported,
            .int => {},
            .float => l = try ig.current_builder.unary(.itof, l),
            .bool => r = try ig.current_builder.unary(.btoi, r),
            .str => {
                if (token_tag != .asterisk) return error.Unsupported;
                // reorder so typeOf works correctly
                const tmp = l;
                l = r;
                r = tmp;
            },
            else => unreachable, // TODO: implement
        },
        .float => switch (rty) {
            .nonetype => return error.Unsupported,
            .int => r = try ig.current_builder.unary(.itof, r),
            .float => {},
            .bool => {
                r = try ig.current_builder.unary(.btoi, r);
                r = try ig.current_builder.unary(.itof, r);
            },
            else => unreachable, // TODO: implement
        },
        .bool => switch (rty) {
            .nonetype => return error.Unsupported,
            .int => l = try ig.current_builder.unary(.btoi, l),
            .float => {
                l = try ig.current_builder.unary(.btoi, l);
                l = try ig.current_builder.unary(.itof, l);
            },
            .bool => {
                l = try ig.current_builder.unary(.btoi, l);
                r = try ig.current_builder.unary(.btoi, r);
            },
            .str => {
                if (token_tag != .asterisk) return error.Unsupported;
                l = try ig.current_builder.unary(.btoi, l);
                // reorder so typeOf works correctly
                const tmp = l;
                l = r;
                r = tmp;
            },
            else => unreachable, // TODO: implement
        },
        .str => switch (rty) {
            .nonetype => return error.Unsupported,
            .int => if (token_tag != .asterisk) return error.Unsupported,
            .bool => {
                if (token_tag != .asterisk) return error.Unsupported;
                r = try ig.current_builder.unary(.btoi, r);
            },
            .str => if (token_tag != .plus) return error.Unsupported,
            else => unreachable,
        },
        // .any => {
        //     if (rty != .any) r = try ig.current_builder.unary(.any, r);
        // },
        else => unreachable, // TODO: implement
    }

    const tag: Ir.Inst.Tag = switch (token_tag) {
        .plus, .plus_equal => .add,
        .minus, .minus_equal => .sub,
        .asterisk, .asterisk_equal => .mul,
        .slash, .slash_slash, .slash_equal, .slash_slash_equal => .div,
        .percent, .percent_equal => .mod,
        .asterisk_asterisk, .asterisk_asterisk_equal => .pow,
        .equal_equal => .eq,
        .bang_equal => .ne,
        .l_angle => .lt,
        .r_angle => .gt,
        .l_angle_equal => .le,
        .r_angle_equal => .ge,
        .ampersand, .ampersand_equal => .band,
        .pipe, .pipe_equal => .bor,
        .caret, .caret_equal => .bxor,
        .l_angle_l_angle, .l_angle_l_angle_equal => .sll,
        .r_angle_r_angle, .r_angle_r_angle_equal => .sra,
        else => unreachable,
    };

    // bitwise operators don't work on floats
    if (ig.typeOf(l) == .float) {
        switch (token_tag) {
            .ampersand,
            .pipe,
            .caret,
            .l_angle_l_angle,
            .r_angle_r_angle,
            => return error.Unsupported,
            else => {},
        }
    }

    return ig.current_builder.binary(tag, l, r);
}

fn logicalOr(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const binary = ig.tree.data(node).binary;

    // always evaluate left, if its true, don't bother with right
    // we need two blocks - one for evaluating the right, and one for exit
    const r_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    const l = try ig.valExpr(scope, binary.left);
    _ = try ig.current_builder.br(l, exit_builder.index, r_builder.index);
    const l_block = try ig.current_builder.seal();

    ig.current_builder = r_builder;
    const r = try ig.valExpr(scope, binary.right);
    _ = try ig.current_builder.jmp(exit_builder.index);
    const r_block = try ig.current_builder.seal();

    ig.current_builder = exit_builder;
    const phi = try ig.current_builder.phi(.bool, l, l_block, r, r_block);
    return phi;
}

fn logicalAnd(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const binary = ig.tree.data(node).binary;

    // always evaluate left, if its false, don't bother with right
    // we need two blocks - one for evaluating the right, and one for exit
    const r_builder = try ig.createBlock();
    const exit_builder = try ig.createBlock();

    const l = try ig.valExpr(scope, binary.left);
    const l_not = try ig.current_builder.unary(.lnot, l);
    _ = try ig.current_builder.br(l_not, exit_builder.index, r_builder.index);
    const l_block = try ig.current_builder.seal();

    ig.current_builder = r_builder;
    const r = try ig.valExpr(scope, binary.right);
    _ = try ig.current_builder.jmp(exit_builder.index);
    const r_block = try ig.current_builder.seal();

    ig.current_builder = exit_builder;
    const phi = try ig.current_builder.phi(.bool, l, l_block, r, r_block);
    return phi;
}

fn unaryExpr(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const op_token = ig.tree.mainToken(node);
    const unary = ig.tree.data(node).unary;

    const operand = try ig.valExpr(scope, unary);
    const tag: Ir.Inst.Tag = switch (ig.tree.tokenTag(op_token)) {
        .plus => return operand,
        .minus => .neg,
        .tilde => .binv,
        .k_not => .lnot,
        else => unreachable,
    };

    return ig.current_builder.unary(tag, operand);
}

fn returnNone(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    _ = scope;
    _ = node;
    const operand = try ig.current_builder.constant(.none);
    return ig.current_builder.unary(.ret, operand);
}

fn returnVal(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const return_val = ig.tree.data(node).return_val;
    const operand = try ig.valExpr(scope, return_val);
    return ig.current_builder.unary(.ret, operand);
}

fn function(ig: *IrGen, scope: *Scope, node: Node.Index) !Ir.Index {
    const module = scope.cast(Scope.Module).?;
    const function_data = ig.tree.data(node).function;
    const signature = ig.tree.extraData(Node.FunctionSignature, function_data.signature);

    const def_token = ig.tree.mainToken(node);
    const ident_token: Ast.TokenIndex = @enumFromInt(@intFromEnum(def_token) + 1);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    var return_type: InternPool.Index = .any;
    if (signature.ret != .null) return_type = try ig.typeExpr(scope, signature.ret);

    // "forward" declare the function in case its recursive
    // TODO: this doesn't work for mutually recursive functions
    const context = scope.context();
    switch (context.tag) {
        // global variables are implemented using a runtime hashtable lookup
        // (which will eventually be wrapped by an inline cache)
        .module => {
            // TODO: use contextPtr
            // TODO: use full function type
            var object = ig.pool.get(module.context).ty;
            object = try object.objectInsert(
                ig.arena,
                .{ .name = id, .ty = return_type },
            );
            module.context = try ig.pool.put(.{ .ty = object });
        },
        // local variables are zero cost, we just map the identifier
        // to the expression value
        // .function => {
        //     const b = scope.cast(Scope.Block).?;
        //     try b.vars.put(ig.arena, id, val);
        //     return val;
        // },
        .block => unreachable,
        else => unreachable,
    }

    const findex = try ig.pool.createFunction(.{
        .tree = ig.tree,
        .node = node,
        .return_type = return_type,
        .global = module.context,
        .ir = undefined,
        .bytecode = undefined,
        .state = .lazy,
    });
    std.debug.print("storing: {}\n", .{module.context});
    const ip = try ig.pool.put(.{ .function = findex });

    const val = try ig.current_builder.constant(ip);
    const object = ig.pool.get(module.context).ty;
    const slot = object.objectSlot(id).?;
    const global_ptr = try ig.current_builder.contextPtr(slot);
    return ig.current_builder.store(global_ptr, val);
}

fn subscript(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const access = ig.tree.data(node).subscript;
    const operand = try ig.valExpr(scope, access.operand);
    const index = try ig.valExpr(scope, access.index);
    const element_ptr = try ig.current_builder.elementPtr(operand, index);
    return switch (ri.semantics) {
        .ptr => element_ptr,
        .val => unreachable, //ig.current_builder.load(element_ptr),
    };
}

fn attribute(ig: *IrGen, scope: *Scope, ri: ResultInfo, node: Node.Index) !Ir.Index {
    const data = ig.tree.data(node).attribute;
    const main_token = ig.tree.mainToken(node);
    const ident_token: Ast.TokenIndex = @enumFromInt(@intFromEnum(main_token) + 1);
    const ident_str = ig.tree.tokenString(ident_token);
    const id = try ig.pool.put(.{ .str = ident_str });

    const object = try ig.valExpr(scope, data);
    const attribute_ptr = try ig.current_builder.attributePtr(object, id);
    return switch (ri.semantics) {
        .ptr => attribute_ptr,
        .val => unreachable, // ig.current_builder.load(attribute_ptr),
    };
}

fn call(ig: *IrGen, scope: *Scope, node: Node.Index) error{ OutOfMemory, Unsupported }!Ir.Index {
    const call_data = ig.tree.data(node).call;
    const slice = ig.tree.extraData(Node.ExtraSlice, call_data.args);
    const ast_args: []const Node.Index = @ptrCast(ig.tree.extraSlice(slice));

    const ptr = try ig.valExpr(scope, call_data.ptr);
    const scratch_top = ig.scratch.items.len;
    defer ig.scratch.shrinkRetainingCapacity(scratch_top);
    try ig.scratch.ensureUnusedCapacity(ig.arena, ast_args.len + 1);
    // if the call is on an attribute access, implicitly insert the attribute operand
    // as an argument
    // now push the visible arguments
    for (ast_args) |ast_arg| {
        const arg = try ig.valExpr(scope, ast_arg);
        ig.scratch.appendAssumeCapacity(@intFromEnum(arg));
    }

    const args = ig.scratch.items[scratch_top..];
    return ig.current_builder.call(ptr, @ptrCast(args));
}

pub fn getTempIr(ig: *const IrGen) Ir {
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
    const data = ig.tree.data(node);
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
