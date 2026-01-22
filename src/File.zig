const std = @import("std");

const Self = @This();

const SinglyLinkedList = @import("linked_list.zig").SinglyLinkedList;
const Line = @import("Line.zig");

lines: SinglyLinkedList(Line) = .{},

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.lines.clearAndFree(allocator);
}

pub fn create(allocator: std.mem.Allocator, input: []const u8) !Self {
    var lines: SinglyLinkedList(Line) = .{};
    var it = std.mem.splitScalar(u8, input, '\n');
    var line_id: usize = 1;
    while (it.next()) |line| : (line_id += 1) {
        try lines.append(allocator, .{
            .num = line_id,
            // The append function internally dupes the line anyways so it's safe to do a const cast here
            .chars = @constCast(line),
        });
    }
    return .{
        .lines = lines,
    };
}

pub fn merge(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
    // Now we build up the output string from all the lines
    var line_it = self.lines.head;
    var writer_allocating: std.Io.Writer.Allocating = .init(allocator);
    defer writer_allocating.deinit();
    const writer = &writer_allocating.writer;
    while (line_it) |line| {
        try writer.writeAll(line.value.chars);
        if (line.next != null) {
            try writer.writeByte('\n');
        }
        line_it = line.next;
    }
    try writer.flush();
    return allocator.dupe(u8, writer.buffered());
}
