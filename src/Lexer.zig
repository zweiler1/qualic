const std = @import("std");

const Self = @This();

pub const TokenType = enum {
    // other tokens
    hashtag,
    eol,
    eof,
    identifier,

    // constants
    string_literal,
    char_literal,
    number_literal,

    // operators
    l_paren, // (
    r_paren, // )
    l_bracket, // [
    r_bracket, // ]
    l_brace, // {
    r_brace, // }
    dot, // .
    comma, // ,
    colon, // :
    semicolon, // ;
    arrow, // ->
    plus_plus, // ++
    minus_minus, // --
    @"and", // &
    @"or", // |
    exclamation, // !
    plus, // +
    minus, // -
    mult, // *
    slash, // /
    mod, // %
    l_shift, // <<
    r_shift, // >>
    lt, // <
    gt, // >
    assign, // =
    le, // <=
    ge, // >=
    eq, // ==
    neq, // !=
    xor, // ^
    peq, // +=
    mieq, // -=
    mueq, // *=
    deq, // /=
    xeq, // ^=

    // builtin types
    void,
    char,
    double,
    float,
    short,
    int,
    long,

    // type modifyer keywords
    auto,
    signed,
    unsigned,
    @"const",
    sizeof,
    static,
    restrict,
    @"volatile",
    register,

    // loop keywords
    @"for",
    do,
    @"while",
    @"break",
    @"continue",

    // switch keywords
    @"switch",
    default,
    case,

    // branching keywords
    @"if",
    @"else",
    goto,

    // function keywords
    @"extern",
    @"inline",
    @"return",

    // type definition keywords
    typedef,
    @"enum",
    @"struct",
    @"union",
};

pub const Token = struct {
    type: TokenType,
    line: usize,
    column: usize,
    lexeme: []const u8,
};

tokens: std.ArrayList(Token),

pub const empty: Self = .{
    .tokens = .empty,
};

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.tokens.clearAndFree(allocator);
}

pub fn tokenize(self: *Self, input: []const u8, allocator: std.mem.Allocator) !void {
    if (self.tokens.items.len > 0) {
        // If this lexer already contains tokens we do not re-tokenize the input
        // Only if the lexer does not contain any elements, e.g. it's "empty" do we tokenize the input
        return;
    }

    var i: usize = 0;
    var line: usize = 1;
    var line_start: usize = 0;
    while (i < input.len) {
        switch (input[i]) {
            else => {
                // Skip spaces, crash on everything else until now to detect unhandled tokens
                std.debug.assert(input[i] == ' ');
            },
            // other tokens
            '#' => ignoreRestOfLine(input, &i, &line, &line_start),
            '\n' => {
                try self.addToken(allocator, input, &i, line, line_start, .eol, 1);
                line += 1;
                line_start = i + 1;
            },
            // identifier
            'a'...'z', 'A'...'Z', '_' => {
                try self.addIdentifierOrKeyword(allocator, input, &i, line, line_start);
            },
            // string_literal,
            '"' => {
                // Skip the '"'
                i += 1;
                var lit_end: usize = i;
                while (input[lit_end] != '"' or input[lit_end - 1] == '\\') {
                    lit_end += 1;
                }
                try self.addToken(allocator, input, &i, line, line_start, .string_literal, lit_end - i + 1);
            },
            '\'' => {
                var lit_end: usize = i;
                while (input[lit_end] != '\'' and input[lit_end] != '\\') {
                    lit_end += 1;
                }
                try self.addToken(allocator, input, &i, line, line_start, .char_literal, lit_end - i + 1);
            },
            // number_literal,
            '0'...'9' => {
                // Number literals can start with 0b, 0x, 0o etc and end with l, ul, f, d etc
            },
            // operators
            '(' => try self.addToken(allocator, input, &i, line, line_start, .l_paren, 1),
            ')' => try self.addToken(allocator, input, &i, line, line_start, .r_paren, 1),
            '[' => try self.addToken(allocator, input, &i, line, line_start, .l_bracket, 1),
            ']' => try self.addToken(allocator, input, &i, line, line_start, .r_bracket, 1),
            '{' => try self.addToken(allocator, input, &i, line, line_start, .l_brace, 1),
            '}' => try self.addToken(allocator, input, &i, line, line_start, .r_brace, 1),
            '.' => try self.addToken(allocator, input, &i, line, line_start, .dot, 1),
            ',' => try self.addToken(allocator, input, &i, line, line_start, .comma, 1),
            ':' => try self.addToken(allocator, input, &i, line, line_start, .colon, 1),
            ';' => try self.addToken(allocator, input, &i, line, line_start, .semicolon, 1),
            '&' => try self.addToken(allocator, input, &i, line, line_start, .@"and", 1),
            '|' => try self.addToken(allocator, input, &i, line, line_start, .@"or", 1),
            '!' => switch (input[i + 1]) {
                '=' => try self.addToken(allocator, input, &i, line, line_start, .neq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .exclamation, 1),
            },
            '+' => switch (input[i + 1]) {
                '+' => try self.addToken(allocator, input, &i, line, line_start, .plus_plus, 2),
                '=' => try self.addToken(allocator, input, &i, line, line_start, .peq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .plus, 1),
            },
            '-' => switch (input[i + 1]) {
                '>' => try self.addToken(allocator, input, &i, line, line_start, .arrow, 2),
                '-' => try self.addToken(allocator, input, &i, line, line_start, .minus_minus, 2),
                '=' => try self.addToken(allocator, input, &i, line, line_start, .mieq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .minus, 1),
            },
            '*' => switch (input[i + 1]) {
                '=' => try self.addToken(allocator, input, &i, line, line_start, .mueq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .mult, 1),
            },
            '/' => switch (input[i + 1]) {
                '/' => {
                    ignoreRestOfLine(input, &i, &line, &line_start);
                },
                '*' => {
                    i += 2;
                    ignoreUntil(input, &i, &line, &line_start, "*/");
                },
                '=' => try self.addToken(allocator, input, &i, line, line_start, .deq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .slash, 1),
            },
            '%' => try self.addToken(allocator, input, &i, line, line_start, .mod, 1),
            '<' => switch (input[i + 1]) {
                '<' => try self.addToken(allocator, input, &i, line, line_start, .l_shift, 2),
                '=' => try self.addToken(allocator, input, &i, line, line_start, .le, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .lt, 1),
            },
            '>' => switch (input[i + 1]) {
                '>' => try self.addToken(allocator, input, &i, line, line_start, .r_shift, 2),
                '=' => try self.addToken(allocator, input, &i, line, line_start, .ge, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .gt, 1),
            },
            '=' => switch (input[i + 1]) {
                '=' => try self.addToken(allocator, input, &i, line, line_start, .eq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .assign, 1),
            },
            '^' => switch (input[i + 1]) {
                '=' => try self.addToken(allocator, input, &i, line, line_start, .xeq, 2),
                else => try self.addToken(allocator, input, &i, line, line_start, .xor, 1),
            },
        }
        i += 1;
        if (i < input.len) {
            continue;
        } else {
            try self.tokens.append(allocator, .{
                .type = .eof,
                .line = 0,
                .column = 0,
                .lexeme = "",
            });
            break;
        }
    }
}

pub fn printTokens(self: *Self) !void {
    // First pass: compute max widths
    var max_pos: usize = 0;
    var max_type: usize = 0;
    var max_lex: usize = 0;

    // format "line:col" into a small stack buffer to measure length
    for (self.tokens.items) |token| {
        var tmp_buf: [32]u8 = undefined;
        const pos = try std.fmt.bufPrint(&tmp_buf, "{d}:{d}", .{ token.line, token.column });
        if (pos.len > max_pos) max_pos = pos.len;

        const tname = @tagName(token.type);
        if (tname.len > max_type) max_type = tname.len;

        if (token.lexeme.len > max_lex) max_lex = token.lexeme.len;
    }

    for (self.tokens.items) |token| {
        var pos_buf: [32]u8 = undefined;
        const pos = try std.fmt.bufPrint(&pos_buf, "{d}:{d}", .{ token.line, token.column });

        // use named fields so we can pass the runtime width values:
        // format string:
        //   {[pos]s:<[pos_w]}   => left-align pos in width pos_w
        //   {[type]s:<[type_w]} => left-align type in width type_w
        //   {[lex]s}            => lexeme as-is
        std.debug.print(
            "{[pos]s:<[pos_w]} | {[type]s:<[type_w]} | {[lex]s}\n",
            .{
                .pos = pos,
                .pos_w = max_pos,
                .type = @tagName(token.type),
                .type_w = max_type,
                .lex = token.lexeme,
            },
        );
    }
}

fn ignoreRestOfLine(input: []const u8, i: *usize, line: *usize, line_start: *usize) void {
    while (input[i.*] != '\n') {
        i.* += 1;
    }
    line.* += 1;
    line_start.* = i.* + 1;
}

fn ignoreUntil(input: []const u8, i: *usize, line: *usize, line_start: *usize, match: []const u8) void {
    while (!std.mem.eql(u8, input[i.* .. i.* + match.len], match)) {
        if (input[i.*] == '\n') {
            line.* += 1;
            line_start.* = i.* + 1;
        }
        i.* += 1;
    }
}

fn addIdentifierOrKeyword(
    self: *Self,
    allocator: std.mem.Allocator,
    input: []const u8,
    i: *usize,
    line: usize,
    line_start: usize,
) !void {
    var ie: usize = i.*;
    // Get how long the identifier is
    while (input[ie] >= 'a' and input[ie] <= 'z' or input[ie] >= 'A' and input[ie] <= 'Z' or input[ie] == '_') {
        ie += 1;
    }
    const identifier: []const u8 = input[i.*..ie];
    const token_type: ?TokenType = std.meta.stringToEnum(TokenType, identifier);
    try self.tokens.append(allocator, .{
        .type = token_type orelse .identifier,
        .line = line,
        .column = i.* - line_start,
        .lexeme = identifier,
    });
    i.* = ie - 1;
}

fn addToken(
    self: *Self,
    allocator: std.mem.Allocator,
    input: []const u8,
    i: *usize,
    line: usize,
    line_start: usize,
    token_type: TokenType,
    token_size: usize,
) !void {
    std.debug.assert(token_size > 0);
    try self.tokens.append(allocator, .{
        .type = token_type,
        .line = line,
        .column = i.* - line_start,
        .lexeme = input[i.* .. i.* + token_size],
    });
    if (token_size > 1) {
        i.* += token_size - 1;
    }
}
