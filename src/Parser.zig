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

    pub fn eql(self: *const Line, other: *const Line) bool {
        return self.num == other.num;
    }
};

// The 'ChangeMoveFunction' change moves a function from within a struct body after it, for this it
// needs to know where the function starts and ends respectively. For now we only have full lines
// for each function. It also contains in which struct the changed function was contained in and it
// also contains where that struct's body ends so that we can place the function after it. Only
// definitions are allowed to be written inside a struct definition, this means that the function
// line can be a single line
pub const ChangeMoveFunction = struct {
    fn_line: usize,
    fn_name: []const u8,
    struct_end_line: usize,
    struct_type_name: []const u8,
};

// The 'ChangeNamespaceCall' change simply changes a namespace call like `StructType.call(...)` to
// the form of `StructType_call(...)`. This means that it does only need to replace the `.` character
// with a `_` character in the correct position. This is why this change struct only contains the
// line and column information where to find that `.` character.
// It could also be a namespaced function definition like `StructType.call(...) {...` for example
// The same rules apply as with namespaced calls themselves, we simply replace the `.` with a `_`.
pub const ChangeNamespaceCallOrDefinition = struct {
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
    namespace: ChangeNamespaceCallOrDefinition,
    instance: ChangeInstanceCall,

    pub fn deinit(self: *Change, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .move => |move| {
                allocator.free(move.fn_name);
                allocator.free(move.struct_type_name);
            },
            .namespace => {},
            .instance => |instance| {
                allocator.free(instance.type);
                allocator.free(instance.instance);
            },
        }
    }

    pub fn clone(self: *const Change, allocator: std.mem.Allocator) anyerror!Change {
        return switch (self.*) {
            .move => |move| .{
                .move = .{
                    .fn_line = move.fn_line,
                    .fn_name = try allocator.dupe(u8, move.fn_name),
                    .struct_end_line = move.struct_end_line,
                    .struct_type_name = try allocator.dupe(u8, move.struct_type_name),
                },
            },
            .namespace => |namespace| .{
                .namespace = .{
                    .line = namespace.line,
                    .column = namespace.column,
                },
            },
            .instance => |instance| .{
                .instance = .{
                    .line = instance.line,
                    .column = instance.column,
                    .type = try allocator.dupe(u8, instance.type),
                    .instance = try allocator.dupe(u8, instance.instance),
                },
            },
        };
    }
};

// This parser owns the lexer instance
lexer: Lexer,

// A list of all changes which need to be done to the source code
changes: SinglyLinkedList(Change),

pub fn init(allocator: std.mem.Allocator, file_content: []const u8) !Self {
    var lexer: Lexer = .init(file_content);
    try lexer.tokenize(allocator);
    return .{
        .lexer = lexer,
        .changes = .{},
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.lexer.deinit(allocator);
    self.changes.clearAndFree(allocator);
}

pub fn parse(self: *Self, allocator: std.mem.Allocator) !void {
    try self.parseStructFunctions(allocator);
    try self.parseCalls(allocator);
}

pub fn parseStructFunctions(self: *Self, allocator: std.mem.Allocator) !void {
    const StructScope = struct {
        name: []const u8,
        definition_line: usize,
        definition_column: usize,
    };
    var in_struct_scope: ?StructScope = null;

    var struct_change_list: std.ArrayList(ChangeMoveFunction) = .empty;
    defer struct_change_list.deinit(allocator);

    const tokens = self.lexer.tokens.items;
    for (tokens, 0..) |token, i| {
        // Ignore all tokens which are not inside a struct scope
        if (in_struct_scope == null and token.type != .@"struct") {
            continue;
        }
        if (in_struct_scope == null) {
            // ensure there are at least two more tokens: identifier and lbrace
            std.debug.assert(token.type == .@"struct");

            if (i + 2 > tokens.len) {
                // not enough tokens to form "struct IDENT {"
                continue;
            }

            // Look at the next two tokens
            if (tokens[i + 1].type != .identifier) {
                // It's not a named struct definition
                continue;
            }
            if (tokens[i + 2].type != .l_brace) {
                // It's not a definition, but maybe just a variable declaration like `struct Type s;`
                continue;
            }

            in_struct_scope = StructScope{
                .name = tokens[i + 1].lexeme,
                .definition_line = token.line,
                .definition_column = token.column,
            };
            continue;
        }
        // We now definitely are inside a struct, now we search for a function scope within the struct
        // If we came to a r_brace then the struct scope ends
        if (token.type == .r_brace) {
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
        // Check if we now are at afunction definition. We need to skip the return type and skip forward
        // until we reach the pattern `identifier(`. When we reached that pattern then we are at the
        // beginning of a function definition. This means that this whole line is assumed to be the
        // function definition line. Inside a struct there are no function calls normally, so this
        // is fine for now.
        if (token.type != .identifier) {
            continue;
        }
        std.debug.assert(i + 1 < tokens.len);
        if (tokens[i + 1].type != .l_paren) {
            // Not a function definition
            continue;
        }
        std.debug.assert(in_struct_scope != null);
        try struct_change_list.append(allocator, .{
            .fn_line = token.line,
            .fn_name = token.lexeme,
            .struct_type_name = in_struct_scope.?.name,
            .struct_end_line = 0, // Unknown until now
        });
    }
}

pub fn parseCalls(self: *Self, allocator: std.mem.Allocator) !void {
    _ = self;
    _ = allocator;
}

/// Caller owns the returned string
pub fn apply(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
    var lines: SinglyLinkedList(Line) = .{};
    defer lines.clearAndFree(allocator);
    var it = std.mem.splitScalar(u8, self.lexer.input, '\n');
    var line_id: usize = 1;
    while (it.next()) |line| : (line_id += 1) {
        // The line itself duplicates the line chars in it's "clone" call
        try lines.append(allocator, .{
            .num = line_id,
            .chars = line,
        });
    }

    var new_lines: SinglyLinkedList(Line) = .{};
    defer new_lines.clearAndFree(allocator);
    var line_it = lines.head;
    // We take the first move change we can find and move all lines from the input to the
    // output until we reach the first move line
    var changes_head = self.changes.head;
    var struct_end_added: std.StringHashMap(void) = .init(allocator);
    defer struct_end_added.deinit();
    while (line_it) |line| {
        if (changes_head) |change| {
            blk: switch (change.value) {
                .move => |move| {
                    if (line.value.num < move.fn_line) {
                        break :blk;
                    }
                    // If the 'struct_end_added' map does not contain an entry for the current struct type
                    // then we need to first add the struct end line to the output lines and remove it from
                    // the input lines all together
                    if (struct_end_added.getKey(move.struct_type_name) == null) {
                        const line_idx: usize = getIdxOfLineNum(lines, move.struct_end_line).?;
                        const end_line = lines.getAt(line_idx).?;
                        try new_lines.append(allocator, end_line.*);
                        try lines.removeAt(allocator, line_idx);
                        try struct_end_added.put(move.struct_type_name, {});
                    }
                    // Check the level of indentation of the function
                    var indent_lvl: usize = 0;
                    const start_line = lines.getAt(getIdxOfLineNum(lines, move.fn_line).?).?;
                    while (start_line.chars[indent_lvl] == ' ') {
                        indent_lvl += 1;
                    }
                    std.debug.assert(line.value.num == move.fn_line);
                    std.debug.assert(line_it != null);
                    // Okay now we need to find and replace the function name with the struct type
                    // followed by an underscore and then the function name like `MyStruct_function`.
                    // And then we need to do the same but also add the `asm("...")` line afterwards,
                    // change the `{` to a `;` and remove all whitespaces in front of the `;`. This
                    // line needs to be inserted first.
                    const new_fn_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ move.struct_type_name, move.fn_name });
                    defer allocator.free(new_fn_name);
                    const line_name_replaced: []const u8 = try std.mem.replaceOwned(u8, allocator, start_line.chars, move.fn_name, new_fn_name);
                    defer allocator.free(line_name_replaced);
                    const last_character: u8 = line_name_replaced[line_name_replaced.len - 1];
                    std.debug.assert(last_character == ';');
                    var line_definition: []const u8 = try allocator.dupe(u8, line_name_replaced);
                    defer allocator.free(line_definition);
                    var line_definition_trimmed = line_definition[0 .. line_definition.len - 1];
                    // Remove all whitespacing and then create the definition name
                    line_definition_trimmed = std.mem.trimEnd(u8, line_definition_trimmed, &std.ascii.whitespace);
                    const formatted_definition_line = try std.fmt.allocPrint(allocator, "{s} asm(\"{s}.{s}\");", .{
                        line_definition_trimmed,
                        move.struct_type_name,
                        move.fn_name,
                    });
                    defer allocator.free(formatted_definition_line);
                    // Now add the definition line with the `asm` formatted at the end
                    try new_lines.append(allocator, .{
                        .num = start_line.num,
                        .chars = formatted_definition_line[indent_lvl..],
                    });
                    // Remove the function definition line from the input so that it id not added twice
                    const line_idx: usize = lines.getIdxOf(&line_it.?.value).?;
                    line_it = line_it.?.next;
                    try lines.removeAt(allocator, line_idx);
                    changes_head = change.next;
                    continue;
                },
                .instance => {
                    std.debug.print("changes_head is 'instance'\n", .{});
                },
                .namespace => {
                    std.debug.print("changes_head is 'namespace'\n", .{});
                },
            }
        }
        try new_lines.append(allocator, line.value);
        line_it = line.next;
    }

    // Now we build up the output string from all the lines
    line_it = new_lines.head;
    // var file: []u8 = undefined;
    // var i: usize = 0;
    // while (line_it) |line| {
    //     if (i == 0) {
    //         file = try allocator.alloc(u8, line.value.chars.len);
    //         @memmove(file[0..], line.value.chars[0..]);
    //     } else {
    //         const old_len: usize = file.len;
    //         file = try allocator.realloc(file, old_len + line.value.chars.len + 1);
    //         file[old_len] = '\n';
    //         std.debug.assert(file.len - old_len - 1 == line.value.chars.len);
    //         @memmove(file[old_len + 1 ..], line.value.chars[0..]);
    //     }
    //     line_it = line.next;
    //     i += 1;
    // }
    // return file;

    var writer_allocating: std.Io.Writer.Allocating = .init(allocator);
    defer writer_allocating.deinit();
    const writer = &writer_allocating.writer;
    while (line_it) |line| {
        try writer.writeAll(line.value.chars);
        try writer.writeByte('\n');
        line_it = line.next;
    }
    try writer.flush();
    return allocator.dupe(u8, writer.buffered());
}

pub fn printChanges(self: *Self) void {
    var i: usize = 0;
    var change_maybe = self.changes.head;
    while (change_maybe) |change| {
        std.debug.print("changes[{d}]: {s}: {{\n", .{ i, @tagName(change.value) });
        switch (change.value) {
            .move => |move| {
                std.debug.print("    fn_line: {d},\n", .{move.fn_line});
                std.debug.print("    fn_name: \"{s}\",\n", .{move.fn_name});
                std.debug.print("    struct_end_line: {d},\n", .{move.struct_end_line});
                std.debug.print("    struct_type_name: \"{s}\"\n", .{move.struct_type_name});
            },
            .instance => {},
            .namespace => {},
        }
        std.debug.print("}}\n", .{});
        i += 1;
        change_maybe = change.next;
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

fn getTokensOfLine(self: *Self, line_num: usize) []Lexer.Token {
    var i: usize = 0;
    while (self.lexer.tokens.items[i].line < line_num) {
        i += 1;
    }
    const start: usize = i;
    while (self.lexer.tokens.items[i].line == line_num) {
        i += 1;
    }
    std.debug.assert(start != i);
    return self.lexer.tokens.items[start..i];
}

fn getIdxOfLineNum(lines: SinglyLinkedList(Line), line_num: usize) ?usize {
    var line_idx: usize = 0;
    var line_it = lines.head;
    while (line_it) |line| {
        if (line.value.num == line_num) {
            return line_idx;
        }
        line_it = line.next;
        line_idx += 1;
    }
    return null;
}
