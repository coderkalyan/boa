const std = @import("std");
const lex = @import("lex.zig");
// const error_handler = @import("error_handler.zig");

const Lexer = lex.Lexer;
const Token = lex.Token;
pub const Ast = @This();

pub const TokenIndex = enum(u32) { unused, _ };
pub const ByteOffset = u32;

// represents the entire, immutable, AST of a source file, once parsed.
// in-progess mutable parsing data is stored in the `Parser` struct in parser.zig
// the AST owns the source, token list, node list, and node extra list
source: [:0]const u8,
tokens: TokenList.Slice,
nodes: std.MultiArrayList(Node).Slice,
extra: []u32,
// errors: []const error_handler.SourceError,

// we store an array of token tags and start locations
// to reference during parsing. AST nodes don't store tokens
// or strings themselves, but rather the index to the relevant
// token(s) in this array. note that tokens themselves don't own
// any strings, but reference character offsets in the global
// source array.
pub const TokenList = std.MultiArrayList(Token);

// represents a node in the femto abstract syntax tree.
// due to the u32::max cap on source file size, we can
// also cap the AST at u32::max nodes. hence, nodes are
// inserted into a flat, contigious arraylist. node pointers
// use u32 indices instead of pointer types. this reduces
// memory footprint and improves cache locality
//
// the AST node is a binary tree because the vast majority
// of nodes only need two children (or fewer) and adding more
// would be a waste. nodes that need to reference more children
// use the extra array explained below in the `Ast` struct
pub const Node = struct {
    // index to the "main" token representing this node, if applicable.
    // examples include 'fn' for function declarations, `let` for
    // const or var declarations, and operator tokens for unary and
    // binary operations. strings such as identifiers can be extracted
    // from this information using a fixed offset (i.e. +1 for const decls)
    main_token: TokenIndex,

    // up to two u32 child references
    data: Data,

    // each union member can hold up to
    // two u32 child references. these are either
    // `Index` types to index into the nodes array
    // or `ExtraIndex` to index into the extra array
    // zig doesn't have distinct integers, so this isn't
    // strictly type safe, but its done for readability
    //
    // extra data indices represent only the "start" of the
    // unpacked extra data struct in the extra array
    pub const Data = union(enum) {
        // used for the null node at the beginning
        placeholder,

        // a single "named" thing like a variable, field, or type name
        ident,

        // function declaration 'def <name> (params...) -> ret: body'
        // main_token = n/a
        function: struct {
            signature: ExtraIndex,
            body: Index,
        },
        // function parameter (in the declaration/prototype)
        // as opposed to *arguments* at the call site
        // 'argc: u32'
        // main_token = name
        param: Index,
        // struct field
        // field: Index,

        // literals
        // main_token = literal string
        integer_literal, //: u64,
        // main_token = unparsed literal string
        float_literal,
        // main_token = literal string (true or false)
        bool_literal,
        // main_token = k_none
        none_literal,
        // main_token = literal string
        // char_literal,
        string_literal,
        // struct_literal: struct {
        //     struct_type: Node.Index,
        //     fields: ExtraIndex,
        // },
        // field_initializer: Node.Index,
        list_literal: struct {
            elements: ExtraIndex,
        },
        tuple_literal: struct {
            elements: ExtraIndex,
        },

        // complex expressions
        // binary expression 'a [+-*/...] b'
        // main_token = operator token
        // left = left side expression node
        // right = right side expression node
        binary: struct {
            left: Index,
            right: Index,
        },
        // unary prefix expression '[+-!~]a'
        unary: Index,

        // unary postfix expressions
        // function call 'foo(1, 2, 3)'
        // main_token = function name
        // args: ExtraSlice containing argument array
        call: struct {
            ptr: Index,
            args: ExtraIndex,
        },
        // accesses an array/slice/many pointer element by index
        subscript: struct {
            operand: Index,
            index: Index,
        },
        slice_simple: struct {
            operand: Index,
            range: ExtraIndex,
        },
        slice_step: struct {
            operand: Index,
            range: ExtraIndex,
        },
        // accesses a field by name (identifier)
        // main_token = '.'
        attribute: Index,

        // declarations
        // constant declaration 'let x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        // const_decl: struct {
        //     ty: Index,
        //     val: Index,
        // },
        // var declaration 'let mut x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        // var_decl: struct {
        //     ty: Index,
        //     val: Index,
        // },
        // type alias 'type Index = u32'
        // main_token = 'type'
        // type_decl: Index,
        // type alias 'distinct type Index = u32'
        // main_token = 'distinct'
        // distinct_type_decl: Index,

        // statements
        // block '{...}'
        // main_token = '{'
        block: struct {
            stmts: ExtraIndex,
        },

        // variable assignment 'foo = "bar"'
        // main_token = variable name
        // val = value node
        assign_simple: struct {
            ptr: Index,
            val: Index,
        },

        // variable assignment with operator 'foo += 1'
        // main_token = variable name
        assign_binary: struct {
            ptr: Index,
            val: Index,
        },

        // empty return 'return'
        // main_token = 'return'
        return_none: void,
        // return value 'return 5'
        // main_token = 'return'
        return_val: Index,
        del: Index,
        yield_val: Index,

        // simple if statement 'if cond {body}'
        // main_token = 'if'
        // condition = conditional expression node
        // exec_true = block node to execute on true
        if_simple: struct {
            condition: Index,
            exec_true: Index,
        },

        // if else statement 'if cond {} else {}'
        // main_token = 'if'
        // condition = conditional expression node
        // exec = extra index to body blocks
        if_else: struct {
            condition: Index,
            exec: ExtraIndex,
        },

        // chained if-else if statement 'if cond {} else if cond {}'
        // main_token = 'if'
        // condition = conditional expression node
        // chain = extra index to chain information
        if_chain: struct {
            condition: Index,
            chain: ExtraIndex,
        },

        for_loop: struct {
            signature: ExtraIndex,
            body: Index,
        },

        while_loop: struct {
            condition: Index,
            body: Index,
        },

        // loop while condition satisfied 'for cond {}'
        // main_token = 'for'
        // condition = conditional expression node
        // body = body block node
        loop_conditional: struct {
            condition: Index,
            body: Index,
        },

        // traditional range loop while condition satisfied
        // with binding and afterthought 'for let mut i = 0; i < 10; i += 1 {}'
        // main_token = 'for'
        // signature = extra index to loop range signature
        loop_range: struct {
            signature: ExtraIndex,
            body: Index,
        },

        // loop flow control
        pass,
        brk,
        cont,

        // body = body block node
        module: struct {
            stmts: ExtraIndex,
        },
    };

    pub const Index = enum(u32) { null, _ }; // index into nodes array
    pub const ExtraIndex = enum(u32) { _ }; // index into extra array
    pub const Tag = std.meta.Tag(Data);

    // represents a contigious range of nodes (subarray)
    pub const Range = struct {
        start: Index,
        end: Index,
    };

    // functionally identical to above, differentiated for clarity
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    // extra data content
    pub const FunctionSignature = struct {
        params: ExtraIndex,
        ret: Index,
    };

    pub const ForSignature = struct {
        target: Index,
        iterable: Index,
    };

    // if else execution information
    // exec_true = block node to execute if condition is met
    // exec_false = block node to execute if condition is not met
    pub const IfElse = struct {
        exec_true: Index,
        exec_false: Index,
    };

    // chained if else if execution information
    // exec_true = block node to execute if condition is met
    // next = next conditional node in the chain (if, if else, or another if chain)
    pub const IfChain = struct {
        exec_true: Index,
        next: Index,
    };

    // range-based for loop signature
    // binding = variable declaration at beginning of loop
    // condition = boolean expression to check loop continuation
    // afterthought = statement executed at the end of each loop iteration
    pub const RangeSignature = struct {
        binding: Index,
        condition: Index,
        afterthought: Index,
    };

    pub const DeclMetadata = struct {
        ty: Index,
        attrs_start: ExtraIndex,
        attrs_end: ExtraIndex,
    };

    pub const SliceSimple = struct {
        start: Index,
        end: Index,
    };

    pub const SliceStep = struct {
        start: Index,
        end: Index,
        step: Index,
    };
};

pub fn extraData(self: *const Ast, comptime T: type, index: Node.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(self.extra[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(tree: *const Ast, sl: Ast.Node.ExtraSlice) []const u32 {
    const start = @intFromEnum(sl.start);
    const end = @intFromEnum(sl.end);
    return tree.extra[start..end];
}

pub fn tokenString(tree: *const Ast, index: TokenIndex) []const u8 {
    const loc = tree.tokens.items(.loc)[@intFromEnum(index)];
    return tree.source[loc.start..loc.end];
}

pub fn tokenTag(tree: *const Ast, index: TokenIndex) Token.Tag {
    return tree.tokens.items(.tag)[@intFromEnum(index)];
}

pub fn mainToken(tree: *const Ast, node: Node.Index) TokenIndex {
    return tree.nodes.items(.main_token)[@intFromEnum(node)];
}

pub fn data(tree: *const Ast, node: Node.Index) Node.Data {
    return tree.nodes.items(.data)[@intFromEnum(node)];
}

pub fn tag(tree: *const Ast, node: Node.Index) Node.Tag {
    return tree.nodes.items(.data)[@intFromEnum(node)];
}

pub fn locateClosingBrace(tree: *const Ast, open: TokenIndex) TokenIndex {
    var depth: u32 = 1;
    var cur: TokenIndex = open + 1;
    while (true) : (cur += 1) {
        const token_tag = tree.tokens.items(.tag)[cur];
        switch (token_tag) {
            .l_brace => depth += 1,
            .r_brace => {
                depth -= 1;
                if (depth == 0) return cur;
            },
            .eof => unreachable,
            else => {},
        }
    }
}
