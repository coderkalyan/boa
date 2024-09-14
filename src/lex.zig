const std = @import("std");
const Allocator = std.mem.Allocator;

// each token contains a type (tag), start, and end index in the source string
pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: u32,
        end: u32,
    };

    pub const Tag = enum(u8) {
        // lexer flow control
        invalid,
        eof,
        newline,
        indent,
        dedent,

        // literals
        ident,
        str_lit,
        int_lit,
        float_lit,

        // grouping
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,

        // delimiters
        semi,
        colon,
        equal,
        period,
        comma,
        underscore,
        hashtag,
        backslash,
        minus_r_angle,

        // operators
        plus,
        minus,
        asterisk,
        asterisk_asterisk,
        slash,
        slash_slash,
        percent,
        at,
        ampersand,
        pipe,
        caret,
        tilde,
        l_angle_l_angle,
        r_angle_r_angle,
        plus_equal,
        minus_equal,
        asterisk_equal,
        asterisk_asterisk_equal,
        slash_equal,
        slash_slash_equal,
        percent_equal,
        colon_equal,
        at_equal,
        ampersand_equal,
        pipe_equal,
        caret_equal,
        l_angle_l_angle_equal,
        r_angle_r_angle_equal,
        bang_equal,

        /// comparison
        equal_equal,
        l_angle,
        l_angle_equal,
        r_angle,
        r_angle_equal,

        // keywords
        k_false,
        k_none,
        k_true,
        k_and,
        k_as,
        k_assert,
        k_async,
        k_await,
        k_break,
        k_class,
        k_continue,
        k_def,
        k_del,
        k_elif,
        k_else,
        k_except,
        k_finally,
        k_for,
        k_from,
        k_global,
        k_if,
        k_import,
        k_in,
        k_is,
        k_lambda,
        k_nonlocal,
        k_not,
        k_or,
        k_pass,
        k_raise,
        k_return,
        k_try,
        k_while,
        k_with,
        k_yield,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "False", .k_false },
        .{ "None", .k_none },
        .{ "True", .k_true },
        .{ "and", .k_and },
        .{ "as", .k_as },
        .{ "assert", .k_assert },
        .{ "async", .k_async },
        .{ "await", .k_await },
        .{ "break", .k_break },
        .{ "class", .k_class },
        .{ "continue", .k_continue },
        .{ "def", .k_def },
        .{ "del", .k_del },
        .{ "elif", .k_elif },
        .{ "else", .k_else },
        .{ "except", .k_except },
        .{ "finally", .k_finally },
        .{ "for", .k_for },
        .{ "from", .k_from },
        .{ "global", .k_global },
        .{ "if", .k_if },
        .{ "import", .k_import },
        .{ "in", .k_in },
        .{ "is", .k_is },
        .{ "lambda", .k_lambda },
        .{ "nonlocal", .k_nonlocal },
        .{ "not", .k_not },
        .{ "or", .k_or },
        .{ "pass", .k_pass },
        .{ "raise", .k_raise },
        .{ "return", .k_return },
        .{ "try", .k_try },
        .{ "while", .k_while },
        .{ "with", .k_with },
        .{ "yield", .k_yield },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }
};

pub const Lexer = struct {
    // source code being analyzed
    source: [:0]const u8,
    // keeps track of the indent level
    indents: std.ArrayListUnmanaged(u32),
    // current lexer index in source
    index: u32,
    // allows us to maintain an "emit single token per next() call"
    // API even though indent changes can cause multiple dedents
    dedent_queue: u32,

    const State = enum {
        start,
        carriage_return,
        indent,

        // named things
        ident,
        underscore,

        // literals
        str_prefix,
        str_lit,
        radix,
        binary,
        octal,
        decimal,
        hex,
        float_base,
        float_exp,

        // partial operators
        plus,
        minus,
        asterisk,
        slash,
        percent,
        at,
        ampersand,
        pipe,
        caret,
        bang,
        l_angle,
        r_angle,
        l_angle_l_angle,
        r_angle_r_angle,
        asterisk_asterisk,
        slash_slash,
        colon,
        equal,
        period,

        line_comment,
    };

    pub fn init(source: [:0]const u8, arena: Allocator) !Lexer {
        return Lexer.init_index(source, arena, 0);
    }

    pub fn init_index(source: [:0]const u8, arena: Allocator, index: u32) !Lexer {
        // we only parse <= ~4GiB files (u32_max characters)
        std.debug.assert(source.len <= std.math.maxInt(u32)); // TODO: nice error

        var lexer = Lexer{
            .source = source,
            .index = index,
            .indents = .{},
            .dedent_queue = 0,
        };
        try lexer.indents.ensureTotalCapacity(arena, 1 + 4);
        lexer.indents.appendAssumeCapacity(0);

        return lexer;
    }

    pub fn next(self: *Lexer, arena: Allocator) !Token {
        // start of file
        var line_start = self.index == 0;
        // end of LF (Unix) or CR LF (DOS)
        line_start = line_start or (self.source[self.index - 1] == '\n');
        // end of CR (Macintosh), but not between CR and LF
        line_start = line_start or ((self.source[self.index - 1] == '\r') and (self.source[self.index] != '\n'));

        // we have a special state for parsing start of line indents
        var state: State = if (line_start) .indent else .start;

        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        // first, flush the dedent queue
        if (self.dedent_queue > 0) {
            self.dedent_queue -= 1;
            result.tag = .dedent;
            return result;
        }

        // used to calculate the space expansion of indents
        var spaces: u32 = 0;
        // TODO: I don't like this, but its the easiest way to disable
        // the continue expression transitioning out of .indent
        var back = false;

        // finite state machine that parses one character at a time
        // the state starts with .start, where we parse (almost) any
        // character and decide what to do with it
        // or .indent, where we parse whitespaces and do some cursed
        // calculations until we see a non-whitespace.
        // 0 = null terminator
        // single character tokens are returned immediately, while
        // multi character tokens (multi character operators, keywords,
        // and identifiers) require intermediate states

        // if the current character ends a token (inclusive), we increment
        // the index to set the correct end location, and break
        // current characters signaling the end of a previous token (exclusive)
        // break but don't increment the index
        // else, the while loop predicate will increment automatically
        while (true) : (self.index += 1) {
            // TODO: can we remove this? we really should
            if (back) {
                self.index -= 1;
                back = false;
            }
            // switch on the state, and then the current character c
            const c = self.source[self.index];
            switch (state) {
                .start => switch (c) {
                    // eof
                    0 => {
                        if (self.validateEof()) |token| result = token;
                        break;
                    },

                    '\n' => {
                        result.tag = .newline;
                        self.index += 1;
                        break;
                    },
                    '\r' => state = .carriage_return,

                    // whitespace
                    ' ', '\t' => result.loc.start = self.index + 1,

                    // identifier
                    'a'...'e', 'g'...'z', 'A'...'E', 'G'...'Z' => {
                        state = .ident;
                        result.tag = .ident;
                    },
                    'f', 'F' => state = .str_prefix,
                    '_' => {
                        state = .underscore;
                        result.tag = .ident;
                    },

                    // string literal
                    '"', '\'' => {
                        state = .str_lit;
                        result.tag = .str_lit;
                    },
                    // number literal
                    '0' => {
                        state = .radix;
                        // self.int_value = 0;
                    },
                    '1'...'9' => {
                        state = .decimal;
                        // self.int_value = c - '0';
                    },

                    // punctuation
                    ';' => {
                        result.tag = .semi;
                        self.index += 1;
                        break;
                    },
                    ':' => state = .colon,
                    '=' => state = .equal,
                    '.' => state = .period,
                    ',' => {
                        result.tag = .comma;
                        self.index += 1;
                        break;
                    },
                    '#' => state = .line_comment,
                    '\\' => {
                        result.tag = .backslash;
                        self.index += 1;
                        break;
                    },

                    '(' => {
                        result.tag = .l_paren;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .r_paren;
                        self.index += 1;
                        break;
                    },
                    '[' => {
                        result.tag = .l_bracket;
                        self.index += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .r_bracket;
                        self.index += 1;
                        break;
                    },
                    '{' => {
                        result.tag = .l_brace;
                        self.index += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .r_brace;
                        self.index += 1;
                        break;
                    },
                    '+' => state = .plus,
                    '-' => state = .minus,
                    '*' => state = .asterisk,
                    '/' => state = .slash,
                    '%' => state = .percent,
                    '@' => state = .at,
                    '&' => state = .ampersand,
                    '|' => state = .pipe,
                    '^' => state = .caret,
                    '~' => {
                        result.tag = .tilde;
                        self.index += 1;
                        break;
                    },
                    '!' => state = .bang,
                    '<' => state = .l_angle,
                    '>' => state = .r_angle,
                    else => {
                        result.tag = .invalid;
                        self.index += 1;
                        break;
                    },
                },
                .carriage_return => switch (c) {
                    '\n' => {
                        result.tag = .newline;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .newline;
                        break;
                    },
                },
                .indent => switch (c) {
                    // eof
                    0 => {
                        if (self.validateEof()) |token| result = token;
                        break;
                    },
                    ' ' => spaces += 1,
                    '\t' => spaces = (spaces + 7) / 8 * 8,
                    '#' => state = .line_comment,
                    // TODO: support other types of return (CR)
                    '\n' => {
                        spaces = 0;
                        state = .indent;
                        result.loc.start = self.index + 1;
                    },
                    else => {
                        const top = self.indents.items[self.indents.items.len - 1];
                        if (spaces > top) {
                            // to improve the API, we have the caller reserve
                            // space in this indents stack.
                            try self.indents.append(arena, spaces);
                            result.tag = .indent;
                            break;
                        } else if (spaces < top) {
                            // queue up all the dedents we need to emit
                            std.debug.assert(self.dedent_queue == 0);
                            while (self.indents.getLast() > spaces) {
                                std.debug.assert(self.indents.pop() != 0);
                                self.dedent_queue += 1;
                            }

                            // then emit the first one and exit.
                            self.dedent_queue -= 1;
                            result.tag = .dedent;
                            break;
                        } else {
                            state = .start;
                            result.loc.start = self.index;
                            // std.debug.print("'{c}' {}\n", .{ c, self.index });
                            // continue;
                            // TODO: this is gross, but we need to *not*
                            // consume the current character and still get
                            // back to start (not emit a token) so we...
                            // * can't break
                            // * can't use continue (still post increments)
                            // * can't subtract from self.index in case it's 0
                            back = true;
                        }
                    },
                },
                .ident => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        // done with identifier, check if keyword
                        if (Token.getKeyword(self.source[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },
                .str_prefix => switch (c) {
                    '"', '\'' => {
                        state = .str_lit;
                        result.tag = .str_lit;
                    },
                    else => {
                        state = .ident;
                        result.tag = .ident;
                    },
                },
                .str_lit => switch (c) {
                    '"', '\'' => {
                        self.index += 1;
                        break;
                    },
                    else => {},
                },
                .radix => switch (c) {
                    'b' => state = .binary,
                    'o' => state = .octal,
                    'x' => state = .hex,
                    '0'...'9' => {
                        state = .decimal;
                        // self.int_value = c - '0';
                    },
                    '.' => if (self.source[self.index + 1] == '.') {
                        result.tag = .int_lit;
                        break;
                    } else {
                        state = .float_base;
                    },
                    'a', 'c'...'n', 'p'...'w', 'y'...'z', 'A'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .binary => switch (c) {
                    '0'...'1' => {}, //self.int_value = (self.int_value << 1) | ((c - '0') & 1),
                    '_' => {},
                    '2'...'9', 'a'...'z', 'A'...'Z' => {
                        while (true) {
                            self.index += 1;
                            switch (self.source[self.index]) {
                                '2'...'9', 'a'...'z', 'A'...'Z' => {},
                                else => break, // includes EOF
                            }
                        }
                        result.tag = .invalid;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .octal => switch (c) {
                    '0'...'7' => {}, //self.int_value = (self.int_value << 3) | ((c - '0') & 7),
                    '_' => {},
                    '8'...'9', 'a'...'z', 'A'...'Z' => {
                        while (true) {
                            self.index += 1;
                            switch (self.source[self.index]) {
                                '8'...'9', 'a'...'z', 'A'...'Z' => {},
                                else => break, // includes EOF
                            }
                        }
                        result.tag = .invalid;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .decimal => switch (c) {
                    '0'...'9' => {}, //self.int_value = (self.int_value * 10) + (c - '0'),
                    '_' => {},
                    '.', 'e' => state = .float_base,
                    'a'...'d', 'f'...'z', 'A'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .hex => switch (c) {
                    '0'...'9' => {}, //self.int_value = (self.int_value << 4) | ((c - '0') & 0xF),
                    'a'...'f' => {
                        // const digit = (c - 'a' + 10) & 0xF;
                        // self.int_value = (self.int_value << 4) | digit;
                    },
                    'A'...'F' => {
                        // const digit = (c - 'A' + 10) & 0xF;
                        // self.int_value = (self.int_value << 4) | digit;
                    },
                    '_' => {},
                    'g'...'z', 'G'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .float_base => switch (c) {
                    '0'...'9', '_' => {},
                    'e' => {
                        state = .float_exp;
                        switch (self.source[self.index + 1]) {
                            '+', '-' => self.index += 1,
                            0 => {
                                state = .start;
                                break;
                            },
                            else => {},
                        }
                    },
                    'a'...'d', 'f'...'z', 'A'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .float_lit;
                        break;
                    },
                },
                .float_exp => switch (c) {
                    '0'...'9', '_' => {},
                    'a'...'z', 'A'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .float_lit;
                        break;
                    },
                },
                .underscore => switch (c) {
                    'a'...'z', 'A'...'Z' => {
                        result.tag = .ident;
                        state = .ident;
                    },
                    else => {
                        result.tag = .underscore;
                        break;
                    },
                },
                .period => switch (c) {
                    '0'...'9' => {
                        state = .float_base;
                    },
                    else => {
                        result.tag = .period;
                        break;
                    },
                },
                .equal => switch (c) {
                    '=' => {
                        result.tag = .equal_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .equal;
                        break;
                    },
                },
                .plus => switch (c) {
                    '=' => {
                        result.tag = .plus_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .plus;
                        break;
                    },
                },
                .minus => switch (c) {
                    '=' => {
                        result.tag = .minus_equal;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        result.tag = .minus_r_angle;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .minus;
                        break;
                    },
                },
                .asterisk => switch (c) {
                    '=' => {
                        result.tag = .asterisk_equal;
                        self.index += 1;
                        break;
                    },
                    '*' => state = .asterisk_asterisk,
                    else => {
                        result.tag = .asterisk;
                        break;
                    },
                },
                .slash => switch (c) {
                    '=' => {
                        result.tag = .slash_equal;
                        self.index += 1;
                        break;
                    },
                    '/' => state = .slash_slash,
                    else => {
                        result.tag = .slash;
                        break;
                    },
                },
                .percent => switch (c) {
                    '=' => {
                        result.tag = .percent_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .percent;
                        break;
                    },
                },
                .at => switch (c) {
                    '=' => {
                        result.tag = .at_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .at;
                        break;
                    },
                },
                .colon => switch (c) {
                    '=' => {
                        result.tag = .colon_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .colon;
                        break;
                    },
                },
                .ampersand => switch (c) {
                    '=' => {
                        result.tag = .ampersand_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .ampersand;
                        break;
                    },
                },
                .pipe => switch (c) {
                    '=' => {
                        result.tag = .pipe_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .pipe;
                        break;
                    },
                },
                .caret => switch (c) {
                    '=' => {
                        result.tag = .caret_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .caret;
                        break;
                    },
                },
                .bang => switch (c) {
                    '=' => {
                        result.tag = .bang_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .invalid;
                        break;
                    },
                },
                .l_angle => switch (c) {
                    '=' => {
                        result.tag = .l_angle_equal;
                        self.index += 1;
                        break;
                    },
                    '<' => {
                        state = .l_angle_l_angle;
                    },
                    else => {
                        result.tag = .l_angle;
                        break;
                    },
                },
                .r_angle => switch (c) {
                    '=' => {
                        result.tag = .r_angle_equal;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        state = .r_angle_r_angle;
                    },
                    else => {
                        result.tag = .r_angle;
                        break;
                    },
                },
                .l_angle_l_angle => switch (c) {
                    '=' => {
                        result.tag = .l_angle_l_angle_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .l_angle_l_angle;
                        break;
                    },
                },
                .r_angle_r_angle => switch (c) {
                    '=' => {
                        result.tag = .r_angle_r_angle_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .r_angle_r_angle;
                        break;
                    },
                },
                .asterisk_asterisk => switch (c) {
                    '=' => {
                        result.tag = .asterisk_asterisk_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .asterisk_asterisk;
                        break;
                    },
                },
                .slash_slash => switch (c) {
                    '=' => {
                        result.tag = .slash_slash_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .slash_slash;
                        break;
                    },
                },
                .line_comment => switch (c) {
                    '\r' => state = .carriage_return,
                    '\n' => {
                        result.loc.start = self.index + 1;
                        spaces = 0;
                        state = .indent;
                    },
                    0 => {
                        state = .start;
                        break;
                    },
                    else => {},
                },
            }
        }

        if (result.tag == .eof) {
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        return result;
    }

    fn eatInvalidLiteral(self: *Lexer) u32 {
        var length: u32 = 0;

        while (self.index + length < self.source.len) : (length += 1) {
            switch (self.source[self.index + length]) {
                'a'...'z', 'A'...'Z' => {},
                else => {
                    return length;
                },
            }
        }

        return length;
    }

    fn validateEof(self: *Lexer) ?Token {
        if (self.index != self.source.len) {
            self.index += 1;
            return Token{
                .tag = .invalid,
                .loc = .{ .start = self.index - 1, .end = self.index },
            };
        } else if (self.indents.items.len > 1) {
            const indent = self.indents.pop();
            std.debug.assert(indent != 0);

            return Token{
                .tag = .dedent,
                .loc = .{ .start = self.index, .end = self.index + 1 },
            };
        } else {
            return null;
        }
    }
};

fn testLex(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = try Lexer.init(source, allocator);
    for (expected_token_tags) |expected_token_tag| {
        const token = try lexer.next(allocator);
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    const eof = try lexer.next(allocator);
    try std.testing.expectEqual(Token.Tag.eof, eof.tag);
    try std.testing.expectEqual(@as(u32, @intCast(source.len)), eof.loc.start);
    try std.testing.expectEqual(@as(u32, @intCast(source.len)), eof.loc.end);
}

// fn testParseInt(source: [:0]const u8, expected_token_tags: []const Token.Tag, expected_literals: []const u64) !void {
//     var lexer = Lexer.init(source);
//     var literal_i: usize = 0;
//     for (expected_token_tags) |expected_token_tag| {
//         const token = lexer.next();
//         try std.testing.expectEqual(expected_token_tag, token.tag);
//         if (expected_token_tag == .int_lit) {
//             try std.testing.expectEqual(lexer.int_value, expected_literals[literal_i]);
//             literal_i += 1;
//         }
//     }
//     const eof = lexer.next();
//     try std.testing.expectEqual(Token.Tag.eof, eof.tag);
//     try std.testing.expectEqual(@as(u32, @intCast(source.len)), eof.loc.start);
//     try std.testing.expectEqual(@as(u32, @intCast(source.len)), eof.loc.end);
// }

test "identifier" {
    try testLex("x", &.{.ident});
    try testLex("abc", &.{.ident});
    try testLex("a123", &.{.ident});
    try testLex("123a", &.{.invalid});
    try testLex("1abc", &.{.invalid});
}

test "string literal" {
    try testLex("\"\"", &.{.str_lit});
    try testLex("''", &.{.str_lit});

    try testLex("\"abc\"", &.{.str_lit});
    try testLex("'abc'", &.{.str_lit});

    try testLex("\"123456\"", &.{.str_lit});
    try testLex("'123456'", &.{.str_lit});

    try testLex("\"let\"", &.{.str_lit});
    try testLex("'let'", &.{.str_lit});

    try testLex("f\"foo\"", &.{.str_lit});
    try testLex("f'foo'", &.{.str_lit});
    try testLex("F\"foo\"", &.{.str_lit});
    try testLex("F'foo'", &.{.str_lit});
}

test "integer literal" {
    // decimal
    try testLex("0", &.{.int_lit});
    try testLex("1", &.{.int_lit});
    try testLex("123", &.{.int_lit});
    try testLex("0123", &.{.int_lit});
    try testLex("000123", &.{.int_lit});
    try testLex("123456789", &.{.int_lit});
    try testLex("123(", &.{ .int_lit, .l_paren });
    try testLex("123;", &.{ .int_lit, .semi });
    try testLex("123abc", &.{.invalid});
    try testLex("123_456", &.{.int_lit});
    try testLex("123_456_789", &.{.int_lit});

    // binary
    try testLex("0b0", &.{.int_lit});
    try testLex("0b1", &.{.int_lit});
    try testLex("0b01", &.{.int_lit});
    try testLex("0b0101011", &.{.int_lit});
    try testLex("0b1234", &.{.invalid});

    // octal
    try testLex("0o0", &.{.int_lit});
    try testLex("0o1", &.{.int_lit});
    try testLex("0o01", &.{.int_lit});
    try testLex("0o0123456", &.{.int_lit});
    try testLex("0o17", &.{.int_lit});
    try testLex("0o178", &.{.invalid});
    try testLex("0o17abc", &.{.invalid});
    try testLex("0o12345_67", &.{.int_lit});
    try testLex("0o12399", &.{.invalid});

    // hex
    try testLex("0x0", &.{.int_lit});
    try testLex("0x1", &.{.int_lit});
    try testLex("0x018ADFF", &.{.int_lit});
    try testLex("0x0123456", &.{.int_lit});
    try testLex("0x789", &.{.int_lit});
    try testLex("0x789A", &.{.int_lit});
    try testLex("0x789a", &.{.int_lit});
    try testLex("0xabcdef", &.{.int_lit});
    try testLex("0xABCDEF", &.{.int_lit});
    try testLex("0x123G", &.{.invalid});
    try testLex("0x12_3456_789_a_b_CDEF", &.{.int_lit});
}

test "float literal" {
    try testLex(".", &.{.period});
    try testLex(".5", &.{.float_lit});
    try testLex("0.5", &.{.float_lit});
    try testLex("1.5", &.{.float_lit});
    try testLex(".51231232", &.{.float_lit});
    try testLex(".51231232e05", &.{.float_lit});
    try testLex(".51231232e+15", &.{.float_lit});
    try testLex(".51231232e-15", &.{.float_lit});
    try testLex(".5_234_12_32e-10", &.{.float_lit});
}

test "grouping punctuation" {
    try testLex(";", &.{.semi});
    try testLex(":", &.{.colon});
    try testLex("=", &.{.equal});
    try testLex(".", &.{.period});
    try testLex(",", &.{.comma});
    try testLex("_", &.{.underscore});
    try testLex("\\", &.{.backslash});
    try testLex("->", &.{.minus_r_angle});
}

test "delimiter punctuation" {
    try testLex("(", &.{.l_paren});
    try testLex(")", &.{.r_paren});
    try testLex("[", &.{.l_bracket});
    try testLex("]", &.{.r_bracket});
    try testLex("{", &.{.l_brace});
    try testLex("}", &.{.r_brace});
}

test "operator punctuation" {
    try testLex("+", &.{.plus});
    try testLex("-", &.{.minus});
    try testLex("*", &.{.asterisk});
    try testLex("**", &.{.asterisk_asterisk});
    try testLex("* *", &.{ .asterisk, .asterisk });
    try testLex("/", &.{.slash});
    try testLex("//", &.{.slash_slash});
    try testLex("/ /", &.{ .slash, .slash });
    try testLex("%", &.{.percent});
    try testLex("@", &.{.at});
    try testLex("&", &.{.ampersand});
    try testLex("|", &.{.pipe});
    try testLex("^", &.{.caret});
    try testLex("~", &.{.tilde});
    try testLex("<<", &.{.l_angle_l_angle});
    try testLex("< <", &.{ .l_angle, .l_angle });
    try testLex(">>", &.{.r_angle_r_angle});
    try testLex("> >", &.{ .r_angle, .r_angle });

    try testLex("+=", &.{.plus_equal});
    try testLex("+ =", &.{ .plus, .equal });
    try testLex("-=", &.{.minus_equal});
    try testLex("- =", &.{ .minus, .equal });
    try testLex("*=", &.{.asterisk_equal});
    try testLex("* =", &.{ .asterisk, .equal });
    try testLex("**=", &.{.asterisk_asterisk_equal});
    try testLex("** =", &.{ .asterisk_asterisk, .equal });
    try testLex("* *=", &.{ .asterisk, .asterisk_equal });
    try testLex("* * =", &.{ .asterisk, .asterisk, .equal });
    try testLex("/=", &.{.slash_equal});
    try testLex("/ =", &.{ .slash, .equal });
    try testLex("//=", &.{.slash_slash_equal});
    try testLex("// =", &.{ .slash_slash, .equal });
    try testLex("/ /=", &.{ .slash, .slash_equal });
    try testLex("/ / =", &.{ .slash, .slash, .equal });
    try testLex("%=", &.{.percent_equal});
    try testLex("% =", &.{ .percent, .equal });
    try testLex(":=", &.{.colon_equal});
    try testLex(": =", &.{ .colon, .equal });
    try testLex("@=", &.{.at_equal});
    try testLex("@ =", &.{ .at, .equal });
    try testLex("&=", &.{.ampersand_equal});
    try testLex("& =", &.{ .ampersand, .equal });
    try testLex("|=", &.{.pipe_equal});
    try testLex("| =", &.{ .pipe, .equal });
    try testLex("^=", &.{.caret_equal});
    try testLex("^ =", &.{ .caret, .equal });
    try testLex("<<=", &.{.l_angle_l_angle_equal});
    try testLex("<< =", &.{ .l_angle_l_angle, .equal });
    try testLex("< <=", &.{ .l_angle, .l_angle_equal });
    try testLex("< < =", &.{ .l_angle, .l_angle, .equal });
    try testLex(">>=", &.{.r_angle_r_angle_equal});
    try testLex(">> =", &.{ .r_angle_r_angle, .equal });
    try testLex("> >=", &.{ .r_angle, .r_angle_equal });
    try testLex("> > =", &.{ .r_angle, .r_angle, .equal });
    try testLex("!=", &.{.bang_equal});
    try testLex("! =", &.{ .invalid, .equal });

    try testLex("==", &.{.equal_equal});
    try testLex("= =", &.{ .equal, .equal });
    try testLex("<", &.{.l_angle});
    try testLex("<=", &.{.l_angle_equal});
    try testLex("< =", &.{ .l_angle, .equal });
    try testLex(">", &.{.r_angle});
    try testLex(">=", &.{.r_angle_equal});
    try testLex("> =", &.{ .r_angle, .equal });
}

test "keywords" {
    try testLex("False", &.{.k_false});
    try testLex("None", &.{.k_none});
    try testLex("True", &.{.k_true});
    try testLex("and", &.{.k_and});
    try testLex("as", &.{.k_as});
    try testLex("assert", &.{.k_assert});
    try testLex("async", &.{.k_async});
    try testLex("await", &.{.k_await});
    try testLex("break", &.{.k_break});
    try testLex("class", &.{.k_class});
    try testLex("continue", &.{.k_continue});
    try testLex("def", &.{.k_def});
    try testLex("del", &.{.k_del});
    try testLex("elif", &.{.k_elif});
    try testLex("else", &.{.k_else});
    try testLex("except", &.{.k_except});
    try testLex("finally", &.{.k_finally});
    try testLex("for", &.{.k_for});
    try testLex("from", &.{.k_from});
    try testLex("global", &.{.k_global});
    try testLex("if", &.{.k_if});
    try testLex("import", &.{.k_import});
    try testLex("in", &.{.k_in});
    try testLex("is", &.{.k_is});
    try testLex("lambda", &.{.k_lambda});
    try testLex("nonlocal", &.{.k_nonlocal});
    try testLex("not", &.{.k_not});
    try testLex("or", &.{.k_or});
    try testLex("pass", &.{.k_pass});
    try testLex("raise", &.{.k_raise});
    try testLex("return", &.{.k_return});
    try testLex("try", &.{.k_try});
    try testLex("while", &.{.k_while});
    try testLex("with", &.{.k_with});
    try testLex("yield", &.{.k_yield});
}

test "line comments" {
    try testLex("#test", &.{});
    try testLex("return # test\ndef", &.{ .k_return, .k_def });
}

// test "arith.fm" {
//     try testLex(@embedFile("tests/arith.fm"), &.{
//         .k_let,   .ident,   .equal, .k_fn,    .l_paren, .r_paren, .ident, .l_brace,
//         .k_let,   .ident,   .equal, .int_lit, .semi,    .k_let,   .ident, .equal,
//         .int_lit, .semi,    .k_let, .ident,   .equal,   .ident,   .plus,  .ident,
//         .semi,    .r_brace, .semi,
//     });
// }
//
// test "fact-iter.fm" {
//     try testLex(@embedFile("tests/fact-iter.fm"), &.{
//         .k_let,   .ident, .equal,          .k_fn,  .l_paren, .ident,         .colon,    .ident, .r_paren, .ident,      .l_brace,
//         .k_let,   .k_mut, .ident,          .colon, .ident,   .equal,         .int_lit,  .semi,  .k_for,   .k_let,      .k_mut,
//         .ident,   .equal, .int_lit,        .semi,  .ident,   .l_angle_equal, .ident,    .semi,  .ident,   .plus_equal, .int_lit,
//         .l_brace, .ident, .asterisk_equal, .ident, .semi,    .r_brace,       .k_return, .ident, .semi,    .r_brace,
//     });
// }
