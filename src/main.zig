const std = @import("std");

const Lexer = @import("Lexer.zig");

// 1. Run the pre-processor stage
//      clang -E -x c -P test.in.qlc -o test.middle.qlc
// 2. Run this transpiler on the pre-processed file
//      qc test.middle.qlc -o test.out.c
// 3. Compile the generated .c file with the compiler of your choice
//      clang test.out.c -o test
//
// These three steps could all be done by `qc` itself maybe...
// so maybe just write `qc test.in.qlc -o test` and it generates the binary?
// Or should it rather only generate `.o` files which need to be linked together? hmmm idk

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const input = "test.in.qlc";
    const middle = "test.middle.qlc";
    var preproc: std.process.Child = .init(&[_][]const u8{
        "clang",
        "-E",
        "-x",
        "c",
        "-P",
        input,
        "-o",
        middle,
    }, allocator);

    // Run the process stage
    _ = try preproc.spawnAndWait();

    // Load the preprocessed file
    const file = try std.fs.cwd().openFile(input, .{});
    defer file.close();
    const stat = try file.stat();
    const file_content = try allocator.alloc(u8, stat.size);
    try file.seekTo(0);
    const bytes_read = try file.readAll(file_content);
    if (bytes_read != stat.size) {
        std.debug.print("Not all bytes read! {d}/{d}\n", .{ bytes_read, stat.size });
    }
    std.debug.print("------ Buffer Start ------\n{s}\n------ Buffer End ------\n\n", .{file_content});

    // Split the preprocessed file by line into an array of slices
    // var lines: std.ArrayList([]const u8) = .empty;
    // defer lines.deinit(allocator);
    //
    // var it = std.mem.splitScalar(u8, buffer, '\n');
    // while (it.next()) |chunk| {
    //     try lines.append(allocator, chunk);
    // }
    //
    // for (lines.items, 0..) |line, i| {
    //     std.debug.print("{d} | {s}\n", .{ i + 1, line });
    // }

    var lexer: Lexer = .empty;
    try lexer.tokenize(file_content, allocator);

    // First pass: compute max widths
    var max_pos: usize = 0;
    var max_type: usize = 0;
    var max_lex: usize = 0;

    for (lexer.tokens.items) |token| {
        // format "line:col" into a small stack buffer to measure length
        var tmp_buf: [32]u8 = undefined;
        const pos = try std.fmt.bufPrint(&tmp_buf, "{d}:{d}", .{ token.line, token.column });
        if (pos.len > max_pos) max_pos = pos.len;

        const tname = @tagName(token.type);
        if (tname.len > max_type) max_type = tname.len;

        if (token.lexeme.len > max_lex) max_lex = token.lexeme.len;
    }

    // Header
    std.debug.print("------ Token Stream Start ------\n", .{});

    // Second pass: print each line using dynamic width via named args
    for (lexer.tokens.items) |token| {
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

    std.debug.print("------ Token Stream End ----\n", .{});
}
