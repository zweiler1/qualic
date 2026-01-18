const std = @import("std");

const Lexer = @import("Lexer.zig");

const Self = @This();

// The 'ChangeMoveFunction' change moves a function from within a struct body after it, for this it
// needs to know where the function starts and ends respectively. For now we only have full lines
// for each function. It also contains in which struct the changed function was contained in and it
// also contains where that struct's body ends so that we can place the function after it.
pub const ChangeMoveFunction = struct {
    fn_start_line: usize,
    fn_end_line: usize,
    struct_end_line: usize,
    struct_type_name: []const u8,
};

// The 'ChangeNamespaceCall' change simply changes a namespace call like `StructType.call(...)` to
// the form of `StructType_call(...)`. This means that it does only need to replace the `.` character
// with a `_` character in the correct position. This is why this change struct only contains the
// line and column information where to find that `.` character.
pub const ChangeNamespaceCall = struct {
    line: usize,
    column: usize,
};

// The 'ChangeInstanceCall' change is a bit more complex, because when parsing it we need to find the
// type of the instance in the pattern like `s.call(...)` we need to analyze the scope the variable
// `s` is in to find it's type, but because the variable can only be of type struct it is a bit easier
// to find out where it's defined, just search for that variable in the given scope with an occurrence
// where it is prefixed with one of the known scoping-struct types, even if it's defined like
// `struct StructType s` we can still simply look at the identifier right in front of it. This makes
// it a bit easier overall.
// The `ChangeInstanceCall` struct contains the information of which type the instance is, where the
// instance call starts (line + column) and the name of the instance variable itself.
pub const ChangeInstanceCall = struct {
    line: usize,
    column: usize,
    type: []const u8,
    instance: []const u8,
};

// A change is just a small modification which needs to be done to the input.
// A change can be one of these operations:
//     - Moving out a function of a struct body and changing it's name accordingly
//     - Changing a namespaced call like `StructType.call(...)` to `StructType_call(...)`
//     - Changing an instance call like `s.call(...)` to `StructType_call(&s, ...)`
//     - (defer stuff is deferred)
pub const Change = union {
    move: ChangeMoveFunction,
    namespace: ChangeNamespaceCall,
    instance: ChangeInstanceCall,
};

// A non-owning list of all tokens this parser works on
tokens: []Lexer.Token,

// A list of all changes which need to be done to the source code
changes: std.ArrayList(Change),

pub fn init(tokens: []Lexer.Token) Self {
    return .{
        .tokens = tokens,
        .changes = .empty,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.changes.clearAndFree(allocator);
}

pub fn parse(self: *Self, allocator: std.mem.Allocator) !void {
    _ = self;
    _ = allocator;
    // TODO: actually parse all changes
}

pub fn apply(self: *Self) []const u8 {
    _ = self;
    // TODO: apply all the collected changes from the first stage
    // We first apply the smaller changes like the call changes and then we apply the move changes
}
