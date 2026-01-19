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

    // Resolve all functions within structs
    var struct_parser: Parser = try .init(allocator, file_content, .struct_functions);
    defer struct_parser.deinit(allocator);
    try struct_parser.parse(allocator);
    std.debug.print("\n------ Struct Parser Changes Start ------\n", .{});
    struct_parser.printChanges();
    std.debug.print("------ Struct Parser Changes End ------\n", .{});
    // Apply all the changes and get the combined file back
    const struct_parser_output: []const u8 = try struct_parser.apply(allocator);
    std.debug.print("\n------ Struct Parser Output Start ------\n{s}------ Struct Parser Output End ------\n", .{struct_parser_output});

    // Resolve all calls within functions
    var call_parser: Parser = try .init(allocator, struct_parser_output, .calls);
    defer call_parser.deinit(allocator);
    try call_parser.parse(allocator);
    std.debug.print("\n------ Call Parser Changes Start ------\n", .{});
    struct_parser.printChanges();
    std.debug.print("------ Call Parser Changes End ------\n", .{});
    // Apply all the changes and get the combined file back
    const call_parser_output: []const u8 = try call_parser.apply(allocator);
    std.debug.print("\n------ Call Parser Output Start ------\n{s}------ Call Parser Output End ------\n", .{call_parser_output});
}
