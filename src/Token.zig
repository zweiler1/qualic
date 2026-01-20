type: Type,
line: usize,
column: usize,
lexeme: []const u8,

pub const Type = enum {
    // custom keywords
    @"defer",

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
