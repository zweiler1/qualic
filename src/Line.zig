//! A line is simple, it has a number and it owns a slice of characters in that line
const std = @import("std");

const Self = @This();

num: usize,
chars: []u8,

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.chars);
}

pub fn clone(self: *const Self, allocator: std.mem.Allocator) anyerror!Self {
    return .{
        .num = self.num,
        .chars = try allocator.dupe(u8, self.chars),
    };
}

pub fn eql(self: *const Self, other: *const Self) bool {
    return self.num == other.num;
}
