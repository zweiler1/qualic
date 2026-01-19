const std = @import("std");

const Lexer = @import("Lexer.zig");
const SinglyLinkedList = @import("linked_list.zig").SinglyLinkedList;

const Self = @This();

// A line is simple, it has a number and it owns a slice of characters in that line
pub const Line = struct {
    num: usize,
    chars: []u8,

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
    fn_name: []const u8,
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
                allocator.free(instance.fn_name);
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
                    .fn_name = try allocator.dupe(u8, instance.fn_name),
                },
            },
        };
    }
};

// This parser owns the lexer instance
lexer: Lexer,

// A list of all changes which need to be done to the source code
changes: SinglyLinkedList(Change),

// A list of all struct types which contain functions
structs_with_fns: std.StringHashMap(void),

pub fn init(allocator: std.mem.Allocator, file_content: []const u8) !Self {
    var lexer: Lexer = .init(file_content);
    try lexer.tokenize(allocator);
    try lexer.printTokens();
    return .{
        .lexer = lexer,
        .changes = .{},
        .structs_with_fns = .init(allocator),
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.lexer.deinit(allocator);
    self.changes.clearAndFree(allocator);
    self.structs_with_fns.deinit();
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
        try self.structs_with_fns.put(in_struct_scope.?.name, {});
    }
}

pub fn parseCalls(self: *Self, allocator: std.mem.Allocator) !void {
    // The begin token of the current function we are in. Needed for the instance calls since they
    // need to resolve types. If it's null it means we are not inside a function definition right
    // now, if it has a value it's the index of the token in the lexer.tokens.items slice where the
    // function starts at.
    // var function_begin: ?usize = null;

    // A small hash map containing the type of the variable as a string view into the lexer's input
    var global_variables: std.StringHashMap([]const u8) = .init(allocator);
    defer global_variables.deinit();

    const ScopeVariable = struct {
        // The name of the variable
        name: []const u8,
        // The type of the variable
        type: []const u8,
        // The "scope level" in which the variable was defined, for example in a sub-scope of the
        // function. This is needed for proper tracking of scopes when variables in sub-scopes go
        // out of scope, for example
        scope_level: usize,
    };
    // A small hash map containing the type of the variable as a string view into the lexer's input
    // It contains all the variables located inside of a function's scope
    // Variables near the end of the list appear in deeper nested scopes while variables near the front
    // appear in earlier scopes.
    var scope_variables: std.ArrayList(ScopeVariable) = .empty;
    defer scope_variables.deinit(allocator);

    const tokens = self.lexer.tokens.items;
    // A scope level of 0 means that we are at the global scope. 1 means we are inside a function's
    // scope and values above 1 mean we are in sub-scopes of a function. Other definitions, like
    // struct, enum or union definitions are not counted as scopes by this algorithm.
    var scope_level: usize = 0;
    for (tokens, 0..) |token, i| {
        // For now we search only for the pattern `identifier.identifier(` where the first
        // identifier is one of the structs containing function definitions. This also means
        // that qualic does not resolve things like `MyStruct.call(` when `MyStruct` does not
        // contain any function definitions at all.
        if (i + 3 > tokens.len) {
            // Skip the tokens at the end
            continue;
        }
        if (token.type == .l_brace) {
            // The scope level increases
            scope_level += 1;
            continue;
        }
        if (token.type == .r_brace) {
            // The scope level decreases, remove all variables of the scope level which now ends
            // TODO: This is the place where the defer would be placed in a similar way to how
            // we can detect that the variable is no longer present in here
            std.debug.assert(scope_level > 0);
            scope_level -= 1;
            while (scope_variables.getLastOrNull()) |last_variable| {
                if (last_variable.scope_level > scope_level) {
                    // The scope variables did not allocate anything so we do not need to free them either
                    _ = scope_variables.pop();
                } else {
                    break;
                }
            }
        }
        if (token.type != .identifier) {
            // Skip if this token is not an identifier
            continue;
        }
        if (tokens[i + 1].type == .identifier and tokens[i + 2].type == .assign) {
            // Check if this token is a known struct type, if it is then we need to add it to the
            // global or scoped variable list
            if (self.structs_with_fns.getKey(token.lexeme) != null) {
                if (scope_level > 0) {
                    // It's a scoped variable
                    try scope_variables.append(allocator, .{
                        .name = tokens[i + 1].lexeme,
                        .type = token.lexeme,
                        .scope_level = scope_level,
                    });
                } else {
                    // It's a global variable
                    try global_variables.put(tokens[i + 1].lexeme, token.lexeme);
                }
                continue;
            }
        }
        if (tokens[i + 1].type != .dot) {
            // The struct type is not followed by a dot
            continue;
        }
        if (tokens[i + 2].type != .identifier) {
            // The struct type is not followed ba a `.identifier`
            continue;
        }
        if (tokens[i + 3].type != .l_paren) {
            // The struct type is not followed by a `.identifier(`
            continue;
        }
        if (self.structs_with_fns.getKey(token.lexeme) != null) {
            // This identifier is a known struct type so this is a namespace call
            // The struct type is now definitely followed by a call, this means that we can add the
            // instance change where the `.` will be changed to an `_`.
            try self.changes.append(allocator, .{
                .namespace = .{
                    .column = tokens[i + 1].column,
                    .line = tokens[i + 1].line,
                },
            });
            // To see if this is a function definition we need to look ahead in a balaced way until
            // we find the closing parenthesis. If we then are followed by an left curly brace then
            // this means that this is the begin of a function definition.
        }
        const global_variable = global_variables.getKey(token.lexeme);
        if (global_variable) |global_var_type| {
            // It's an instance call of a globally known variable of struct type x
            try self.changes.append(allocator, .{
                .instance = .{
                    .line = token.line,
                    .column = token.column,
                    .type = global_var_type,
                    .instance = token.lexeme,
                    .fn_name = tokens[i + 2].lexeme,
                },
            });
            break;
        }
        for (scope_variables.items) |scope_var| {
            if (std.mem.eql(u8, scope_var.name, token.lexeme)) {
                // It's an instance call of a known variable inside the function's scope
                try self.changes.append(allocator, .{
                    .instance = .{
                        .line = token.line,
                        .column = token.column,
                        .type = scope_var.type,
                        .instance = scope_var.name,
                        .fn_name = tokens[i + 2].lexeme,
                    },
                });
                break;
            }
        }
    }
}

/// Caller owns the returned string
pub fn apply(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
    // Sort all the changes
    var changes_head = self.changes.head;
    const changes_len = self.changes.len();
    for (0..changes_len) |i| {
        changes_head = self.changes.head;
        while (changes_head) |change| {
            if (change.*.next) |next| {
                const change_line = switch (change.value) {
                    .move => |move| move.fn_line,
                    .namespace => |namespace| namespace.line,
                    .instance => |instance| instance.line,
                };
                const next_line = switch (next.value) {
                    .move => |move| move.fn_line,
                    .namespace => |namespace| namespace.line,
                    .instance => |instance| instance.line,
                };
                var needs_swapping: bool = false;
                if (next_line < change_line) {
                    // We need to swap them
                    needs_swapping = true;
                } else if (next_line == change_line) {
                    // Sort which column comes first. We *always* resolve *all* namespaces within the
                    // same line before we resolve the instance calls becaues the namespaces are
                    // positional and the instance calls are purely patternal. This means that within
                    // the same line first need to come all namespaces (sorted would be good) and then
                    // all instances (also sorted would be good). They do not *need* to be sorted in
                    // their own, but it's just better that way. This means that we *always* try to
                    // put namespaces in front of instances.
                    needs_swapping = change.value == .instance and next.value == .namespace;
                }
                if (needs_swapping) {
                    const next_next = next.next;
                    change.next = next_next;
                    next.next = change;
                    if (i == 0) {
                        self.changes.head = next;
                    } else {
                        const last = self.changes.getNodeAt(i - 1).?;
                        last.next = next;
                    }
                }
            }
            changes_head = change.next;
        }
    }
    changes_head = self.changes.head;

    // Get all the lines from the lexer's input line by line
    var lines: SinglyLinkedList(Line) = .{};
    defer lines.clearAndFree(allocator);
    var it = std.mem.splitScalar(u8, self.lexer.input, '\n');
    var line_id: usize = 1;
    while (it.next()) |line| : (line_id += 1) {
        // The line itself duplicates the line chars in it's "clone" call
        const line_cpy: []u8 = try allocator.dupe(u8, line);
        defer allocator.free(line_cpy);
        try lines.append(allocator, .{
            .num = line_id,
            .chars = line_cpy,
        });
    }

    // Apply all changes
    var new_lines: SinglyLinkedList(Line) = .{};
    defer new_lines.clearAndFree(allocator);
    var line_it = lines.head;
    // We take the first move change we can find and move all lines from the input to the
    // output until we reach the first move line
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
                    const line_idx: usize = lines.getIdxOf(&line.value).?;
                    line_it = line.next;
                    try lines.removeAt(allocator, line_idx);
                    changes_head = change.next;
                    continue;
                },
                .namespace => |namespace| {
                    if (line.value.num != namespace.line) {
                        break :blk;
                    }
                    // Modify the input line directly without adding it to the output. This way other
                    // changes to this line (for example from the `instance`) can still take effect.
                    line.value.chars[namespace.column] = '_';
                    changes_head = change.next;
                    continue;
                },
                .instance => |instance| {
                    if (line.value.num != instance.line) {
                        break :blk;
                    }
                    // Modify the input line directly without adding it to the output. This way other
                    // changes to this line can still take effect.
                    const instance_fmt_empty = try std.fmt.allocPrint(allocator, "{s}.{s}()", .{
                        instance.instance,
                        instance.fn_name,
                    });
                    defer allocator.free(instance_fmt_empty);
                    const replace_fmt_empty = try std.fmt.allocPrint(allocator, "{s}_{s}(&{s})", .{
                        instance.type,
                        instance.fn_name,
                        instance.instance,
                    });
                    defer allocator.free(replace_fmt_empty);
                    const new_line_empty = try std.mem.replaceOwned(
                        u8,
                        allocator,
                        line.value.chars,
                        instance_fmt_empty,
                        replace_fmt_empty,
                    );
                    defer allocator.free(new_line_empty);

                    const instance_fmt = try std.fmt.allocPrint(allocator, "{s}.{s}(", .{
                        instance.instance,
                        instance.fn_name,
                    });
                    defer allocator.free(instance_fmt);
                    const replace_fmt = try std.fmt.allocPrint(allocator, "{s}_{s}(&{s}, ", .{
                        instance.type,
                        instance.fn_name,
                        instance.instance,
                    });
                    defer allocator.free(replace_fmt);
                    const new_line = try std.mem.replaceOwned(u8, allocator, new_line_empty, instance_fmt, replace_fmt);
                    allocator.free(line.value.chars);
                    line.value.chars = new_line;
                    changes_head = change.next;
                    continue;
                },
            }
        }
        try new_lines.append(allocator, line.value);
        line_it = line.next;
    }

    // Now we build up the output string from all the lines
    line_it = new_lines.head;
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
            .namespace => |namespace| {
                std.debug.print("    line: {d},\n", .{namespace.line});
                std.debug.print("    column: {d},\n", .{namespace.column});
            },
            .instance => |instance| {
                std.debug.print("    line: {d},\n", .{instance.line});
                std.debug.print("    column: {d},\n", .{instance.column});
                std.debug.print("    type: {s},\n", .{instance.type});
                std.debug.print("    name: \"{s}\",\n", .{instance.instance});
                std.debug.print("    fn_name: \"{s}\",\n", .{instance.fn_name});
            },
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
