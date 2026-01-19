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
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(debug_allocator.deinit() == .ok);
    const allocator = debug_allocator.allocator();

    const input = "test.in.qlc";
    const middle = "test.middle.qlc";
    const output = "test.out.c";
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
    defer allocator.free(file_content);
    try file.seekTo(0);
    const bytes_read = try file.readAll(file_content);
    if (bytes_read != stat.size) {
        std.debug.print("Not all bytes read! {d}/{d}\n", .{ bytes_read, stat.size });
    }
    std.debug.print("------ Buffer Start ------\n{s}\n------ Buffer End ------\n", .{file_content});

    // Try to parse and apply all changes in one go
    var parser: Parser = try .init(allocator, file_content);
    defer parser.deinit(allocator);
    try parser.parse(allocator);
    std.debug.print("\n------ Parser Changes Start ------\n", .{});
    parser.printChanges();
    std.debug.print("------ Parser Changes End ------\n", .{});
    // Apply all the changes and get the combined file back
    const parser_output: []const u8 = try parser.apply(allocator);
    defer allocator.free(parser_output);
    std.debug.print("\n------ Parser Output Start ------\n{s}------ Parser Output End ------\n", .{parser_output});

    // Write the parser output into the output file
    const out_file = std.fs.cwd().createFile(output, .{ .truncate = true }) catch |err| {
        std.debug.print("Failed to create file: {}\n", .{err});
        return err;
    };
    defer out_file.close();
    var buf: [1024]u8 = undefined;
    var file_writer = out_file.writer(&buf);
    var writer = &file_writer.interface;
    try writer.writeAll(parser_output);
    try writer.flush();
}
