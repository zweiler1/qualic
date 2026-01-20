const std = @import("std");
const clap = @import("clap");

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

const VERSION = "0.0.1";

const params = clap.parseParamsComptime(
    \\-h, --help             display this help and exit
    \\-d, --dry              does a dry-run, prints to stdout but does not write to any file
    \\-o, --out <OUTPUT>     the output .c file of qualic (default: quali.c)
    \\-p, --preprocess       run the c preprocessor before applying the qualic changes
    \\-v, --version          print the version of the qualic preprocessor
    \\-V, --verbose          prints a lot of in-between output and debug information
    \\<INPUT>                the .qlc file to "transpile" (pre-process) to a .c output file
);

pub const Options = struct {
    dry: bool = false,
    output: []const u8 = "quali.c",
    input: []const u8 = undefined,
    preprocess: bool = false,
    verbose: bool = false,
};

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(debug_allocator.deinit() == .ok);
    const allocator = debug_allocator.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer: std.fs.File.Writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer: std.fs.File.Writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr: *std.Io.Writer = &stderr_writer.interface;

    const parsers = comptime .{
        .INPUT = clap.parsers.string,
        .OUTPUT = clap.parsers.string,
    };
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.report(stderr, err) catch {};
        return err;
    };
    defer res.deinit();

    // --help called
    if (res.args.help != 0) {
        try usage(stdout);
        return stdout.flush();
    }

    // --version called
    if (res.args.version != 0) {
        try stdout.print("qualic v{s}\n", .{VERSION});
        return stdout.flush();
    }

    // no arguments provided
    if (res.positionals[0] == null) {
        try stderr.print("qualic: missing input file\nTry 'qualic --help' for more information.\n", .{});
        try stderr.flush();
        std.process.exit(1);
    }

    const options: Options = .{
        .input = res.positionals[0].?,
        .dry = res.args.dry != 0,
        .output = res.args.out orelse "quali.c",
        .preprocess = res.args.preprocess != 0,
        .verbose = res.args.verbose != 0,
    };
    // std.debug.print("options: {{ .input = \"{s}\", .output = \"{s}\", .preprocess = {any} }}\n", .{
    //     options.input,
    //     options.output,
    //     options.preprocess,
    // });

    const middle = "test.middle.qlc";
    if (options.preprocess) {
        var preproc: std.process.Child = .init(&[_][]const u8{
            "clang",
            "-E",
            "-x",
            "c",
            "-P",
            options.input,
            "-o",
            middle,
        }, allocator);
        // Run the process stage
        _ = try preproc.spawnAndWait();
    }

    // Load the preprocessed file
    const file_to_parse = if (options.preprocess) middle else options.input;
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
    if (options.verbose) {
        std.debug.print("------ Input Start ------\n{s}\n------ Input End ------\n", .{file_content});
    }

    // Try to parse and apply all changes in one go
    var parser: Parser = try .init(allocator, file_content, options);
    defer parser.deinit(allocator);
    try parser.parse(allocator);
    // Apply all the changes and get the combined file back
    const parser_output: []const u8 = try parser.apply(allocator);
    defer allocator.free(parser_output);

    if (options.dry) {
        try stdout.print("{s}", .{parser_output});
        try stdout.flush();
        return;
    }

    // Write the parser output into the output file
    const out_file = std.fs.cwd().createFile(options.output, .{ .truncate = true }) catch |err| {
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

fn usage(stdout: *std.io.Writer) !void {
    try stdout.print(
        \\Usage: qualic INPUT [OPTION]..
        \\
    , .{});

    try clap.help(stdout, clap.Help, &params, .{
        .description_indent = 4,
        .spacing_between_parameters = 0,
        .indent = 2,
        .description_on_new_line = false,
    });

    try stdout.print(
        \\
        \\Examples:
        \\  qualic some_file.qlc                -> outputs a `quali.c` file
        \\  qualic some_file.qlc -o out.c       -> outputs a `out.c` file
        \\  qualic some_file.qlc -o out.c -p    -> outputs a large `out.c` file with all C macros expanded
        \\
    , .{});
}
