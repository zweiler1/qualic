const std = @import("std");

const Lexer = @import("Lexer.zig");
const SinglyLinkedList = @import("linked_list.zig").SinglyLinkedList;

const Self = @This();

// A line is simple, it has a number and it owns a slice of characters in that line
pub const Line = struct {
    num: usize,
    chars: []const u8,

    pub fn deinit(self: *Line, allocator: std.mem.Allocator) void {
        allocator.free(self.chars);
    }

    pub fn clone(self: *const Line, allocator: std.mem.Allocator) anyerror!Line {
        return .{
            .num = self.num,
            .chars = try allocator.dupe(u8, self.chars),
        };
    }
};

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
pub const Change = union(enum) {
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
    const StructScope = struct {
        name: []const u8,
        definition_line: usize,
        definition_column: usize,
    };
    var in_struct_scope: ?StructScope = null;

    const StructFunctionScope = struct {
        function_name: []const u8,
        definition_line: usize,
        definition_column: usize,
    };
    var in_function_scope: ?StructFunctionScope = null;

    var struct_change_list: std.ArrayList(ChangeMoveFunction) = .empty;
    defer struct_change_list.deinit(allocator);

    for (self.tokens, 0..) |token, i| {
        // Ignore all tokens which are not inside a struct scope
        if (in_struct_scope == null and token.type != .@"struct") {
            continue;
        }
        if (in_struct_scope == null) {
            // ensure there are at least two more tokens: identifier and lbrace
            std.debug.assert(token.type == .@"struct");

            if (i + 2 > self.tokens.len) {
                // not enough tokens to form "struct IDENT {"
                continue;
            }

            // Look at the next two tokens
            if (self.tokens[i + 1].type != .identifier) {
                // It's not a named struct definition
                continue;
            }
            if (self.tokens[i + 2].type != .l_brace) {
                // It's not a definition, but maybe just a variable declaration like `struct Type s;`
                continue;
            }

            in_struct_scope = StructScope{
                .name = self.tokens[i + 1].lexeme,
                .definition_line = token.line,
                .definition_column = token.column,
            };
            continue;
        }
        // We now definitely are inside a struct, now we search for a function scope within the struct
        // If we came to a r_brace then the struct scope ends
        if (token.type == .r_brace) {
            if (in_function_scope != null) {
                // We exit the current function scope *inside* a struct scope. This means that we have
                // found the "full" function which needs to be moved out of the struct scope. We can add
                // the change function here, but we need to add it to a local list first. When we exit
                // the struct we can tell the change where it needs to go, this is why we need to store
                // it in it's own list first.
                std.debug.assert(in_struct_scope != null);
                try struct_change_list.append(allocator, .{
                    .fn_start_line = in_function_scope.?.definition_line,
                    .fn_end_line = token.line,
                    .struct_type_name = in_struct_scope.?.name,
                    .struct_end_line = 0, // Unknown until now
                });
                in_function_scope = null;
                continue;
            }
            if (in_struct_scope != null) {
                // We exit the struct scope, so we need to go through all the function changes until now
                // and "tell" them where they should be inserted at, namely at the end of the struct scope,
                // so right where we are *now*.
                // We also add the changes to the global change list
                for (struct_change_list.items) |*change| {
                    change.struct_end_line = token.line;
                    try self.changes.append(allocator, .{ .move = change.* });
                }
                struct_change_list.clearAndFree(allocator);
                in_struct_scope = null;
                continue;
            }
            // We should either be in a struct or in a function definition so this case should not happen
            std.debug.assert(false);
        }
        if (in_function_scope == null) {
            // Check if we now enter a function scope. We need to skip the return type and skip forward
            // until we reach the pattern `identifier(`. When we reached that pattern then we are at the
            // beginning of a function definition. This means that this whole line is assumed to be the
            // function definition line. Inside a struct there are no function calls normally, so this
            // is fine for now.
            if (token.type != .identifier) {
                continue;
            }
            std.debug.assert(i + 1 < self.tokens.len);
            if (self.tokens[i + 1].type != .l_paren) {
                // Not a call definition
                continue;
            }
            in_function_scope = StructFunctionScope{
                .function_name = token.lexeme,
                .definition_line = token.line,
                .definition_column = token.column,
            };
        }
        // We are inside a function scope, for now we simply do nothing in here
    }
}

pub fn apply(self: *Self, lines: *SinglyLinkedList(Line)) []const u8 {
    _ = self;
    // For now only print all the lines
    var line_it = lines.head;
    while (line_it) |line| {
        std.debug.print("line[{d}]: {s}\n", .{ line.value.num, line.value.chars });
        line_it = line.next;
    }
    return "";
}

pub fn printChanges(self: *Self) void {
    for (self.changes.items, 0..) |change, i| {
        std.debug.print("changes[{d}]: {s}: {{\n", .{ i, @tagName(change) });
        switch (change) {
            .move => |move| {
                std.debug.print("    fn_start_line: {d},\n", .{move.fn_start_line});
                std.debug.print("    fn_end_line: {d},\n", .{move.fn_end_line});
                std.debug.print("    struct_end_line: {d},\n", .{move.struct_end_line});
                std.debug.print("    struct_type_name: \"{s}\"\n", .{move.struct_type_name});
            },
            .instance => {},
            .namespace => {},
        }
        std.debug.print("}}\n", .{});
    }
}

pub fn createHash(input: []const u8, hash: *[8]u8) void {
    const charset: []const u8 = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    const charset_size: u32 = 61;

    hash[0] = '0';
    if (input.len == 0) {
        return;
    }

    var seed: u32 = 2166136261;

    // FNV-1a over bytes; perform multiplication in u64 and truncate
    for (input) |b| {
        seed ^= @intCast(b);
        seed = @truncate(@as(u64, seed) * 16777619);
    }

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        // compute pos_hash with truncating 32-bit behavior
        var pos_hash: u32 = seed ^ @as(u32, @truncate(@as(u64, i) * 0x9E3779B9));

        pos_hash = @truncate(@as(u64, pos_hash) * 0x85EBCA6B);
        pos_hash ^= pos_hash >> 13;
        pos_hash = @truncate(@as(u64, pos_hash) * 0xC2B2AE35);
        pos_hash ^= pos_hash >> 16;

        const idx: usize = @intCast(pos_hash % charset_size);
        hash[@as(usize, @intCast(i))] = charset[idx];

        seed = pos_hash;
    }
}
