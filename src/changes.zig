const std = @import("std");

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
    type: []const u8,
    instance: []const u8,
    fn_name: []const u8,
};

// The 'ChangeDeferStatement' change is a bit more complex. It can only happen within a function,
// the defer keyword is not allowed outside of a function scope. We need to remember all defer
// statements in their correct ordering, remember in which scope level they were defined in, and
// when the scope ends then we need to apply all the defer statements in backwards ordering. This
// is not as hard to do as i initially thought simply because of the ground-work of the instance
// analysis pass. This is the reason why collecting all defer statements is done in the same pass
// as the `ChangeInstanceCall` is.
pub const ChangeDeferStatement = struct {
    line: usize,
    indentation: usize,
    // If content_line_begin == content_line_ned then it's a defer statement like `defer <expr>;`
    // If they differ then it's a defer block like `defer { ... }`
    content_line_begin: usize,
    content_line_end: usize,
};

// The 'ChangeDeferReturnRemove' change is pretty simple. When we come across the `return`
// statement line we do not emit `return <expr>;` but we emit `<type> <return_HASH> = <expr>;`
// that later on, in the 'ChangeDeferReturnInsert' change we can insert the line
// `return <return_HASH>;` This is important because this way the return value is evaluated
// before any defer statements are inserted, potentially freeing the value(s) needed for the return
// expression.
pub const ChangeDeferReturnRemove = struct {
    line: usize,
    fn_type_line: usize,
    // column of type end in line
    fn_type_end: usize,
    var_name: []const u8,
};

// The 'ChangeDeferReturnInsert' change simply emits the line `return <return_HASH>;` with the
// given indentation. The name of the temp variable is the result of a deterministic hashing function
pub const ChangeDeferReturnInsert = struct {
    line: usize,
    indentation: usize,
    var_name: []const u8,
};

// A change is just a small modification which needs to be done to the input.
// A change can be one of these operations:
//     - Moving out a function of a struct body and changing it's name accordingly
//     - Changing a namespaced call like `StructType.call(...)` to `StructType_call(...)`
//     - Changing an instance call like `s.call(...)` to `StructType_call(&s, ...)`
//     - Defer for statements, like `defer <statement>;`
//     - Defer for blocks (not implemented yet)
pub const Change = union(enum) {
    move: ChangeMoveFunction,
    namespace: ChangeNamespaceCallOrDefinition,
    instance: ChangeInstanceCall,
    defer_s: ChangeDeferStatement,
    defer_return_remove: ChangeDeferReturnRemove,
    defer_return_insert: ChangeDeferReturnInsert,

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
            .defer_s => {},
            .defer_return_remove => |remove| {
                allocator.free(remove.var_name);
            },
            .defer_return_insert => |insert| {
                allocator.free(insert.var_name);
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
                    .type = try allocator.dupe(u8, instance.type),
                    .instance = try allocator.dupe(u8, instance.instance),
                    .fn_name = try allocator.dupe(u8, instance.fn_name),
                },
            },
            .defer_s => |defer_s| .{
                .defer_s = .{
                    .line = defer_s.line,
                    .indentation = defer_s.indentation,
                    .content_line_begin = defer_s.content_line_begin,
                    .content_line_end = defer_s.content_line_end,
                },
            },
            .defer_return_remove => |remove| .{
                .defer_return_remove = .{
                    .line = remove.line,
                    .fn_type_line = remove.fn_type_line,
                    .fn_type_end = remove.fn_type_end,
                    .var_name = try allocator.dupe(u8, remove.var_name),
                },
            },
            .defer_return_insert => |insert| .{
                .defer_return_insert = .{
                    .line = insert.line,
                    .indentation = insert.indentation,
                    .var_name = try allocator.dupe(u8, insert.var_name),
                },
            },
        };
    }
};
