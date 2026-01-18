const std = @import("std");

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const SinglyLinkedList = @import("linked_list.zig").SinglyLinkedList;

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
    // 'input' should be 'middle' here, but for easier testing we use 'input' for now
    const file_to_parse = input;
    const file = try std.fs.cwd().openFile(file_to_parse, .{});
    defer file.close();
    const stat = try file.stat();
    const file_content = try allocator.alloc(u8, stat.size);
    try file.seekTo(0);
    const bytes_read = try file.readAll(file_content);
    if (bytes_read != stat.size) {
        std.debug.print("Not all bytes read! {d}/{d}\n", .{ bytes_read, stat.size });
    }
    std.debug.print("------ Buffer Start ------\n{s}\n------ Buffer End ------\n", .{file_content});

    var lexer: Lexer = .init(file_content);
    defer lexer.deinit(allocator);
    try lexer.tokenize(allocator);
    std.debug.print("\n------ Token Stream Start ------\n", .{});
    try lexer.printTokens();
    std.debug.print("------ Token Stream End ----\n", .{});

    var parser: Parser = .init(lexer.tokens.items);
    defer parser.deinit(allocator);
    try parser.parse(allocator);
    std.debug.print("\n------ Changes Start ------\n", .{});
    parser.printChanges();
    std.debug.print("------ Changes End ----\n", .{});

    // Split the preprocessed file by line into an array of lines
    var lines: SinglyLinkedList(Parser.Line) = .{};
    defer lines.clearAndFree(allocator);
    var it = std.mem.splitScalar(u8, file_content, '\n');
    var line_id: usize = 1;
    while (it.next()) |line| {
        // The line itself duplicates the line chars in it's "clone" call
        try lines.append(allocator, .{
            .num = line_id,
            .chars = line,
        });
        line_id += 1;
    }

    // Apply all the changes and get the combined file back
    const transpiled_file: []const u8 = try parser.apply(allocator, &lines);
    std.debug.print("\n------ Transpiled File Start ------\n{s}------ Transpiled File End ------\n", .{transpiled_file});
}
