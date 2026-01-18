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
    defer lexer.deinit(allocator);
    try lexer.tokenize(file_content, allocator);
    std.debug.print("------ Token Stream Start ------\n", .{});
    try lexer.printTokens();
    std.debug.print("------ Token Stream End ----\n", .{});
}
