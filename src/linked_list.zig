const std = @import("std");
const meta = @import("meta.zig");

pub fn SinglyLinkedList(comptime T: type) type {
    return struct {
        pub const Iterator = struct {
            current: ?*Node(T),

            pub fn next(self: *Iterator) ?*T {
                if (self.current) |current| {
                    self.current = current.next;
                    return &current.value;
                }
                return null;
            }
        };

        head: ?*Node(T) = null,

        pub fn clearAndFree(self: *SinglyLinkedList(T), allocator: std.mem.Allocator) void {
            if (self.head) |head| {
                head.clearAndFreeAll(allocator);
                allocator.destroy(head);
                self.head = null;
            }
        }

        pub fn fromSlice(allocator: std.mem.Allocator, slice: []const T) !SinglyLinkedList(T) {
            var list: SinglyLinkedList(T) = .{};
            var i: usize = slice.len;
            while (i > 0) : (i -= 1) {
                try list.prepend(allocator, slice[i - 1]);
            }
            return list;
        }

        pub fn toOwnedSlice(self: *const SinglyLinkedList(T), allocator: std.mem.Allocator) ![]T {
            if (self.head == null) {
                return error.EmptyList;
            }
            var list: []T = try allocator.alloc(T, self.len());
            var current: ?*Node(T) = self.head;
            var i: usize = 0;
            while (current) |node| : (i += 1) {
                list[i] = try node.value.clone(allocator);
                current = node.next;
            }
            return list;
        }

        pub fn append(self: *SinglyLinkedList(T), allocator: std.mem.Allocator, value: T) !void {
            const new_node: *Node(T) = try allocator.create(Node(T));
            new_node.* = .{ .next = null, .value = try value.clone(allocator) };
            if (self.head) |head_value| {
                // Find last element
                var current: *Node(T) = head_value;
                while (current.next) |next_value| {
                    current = next_value;
                }
                current.next = new_node;
            } else {
                self.head = new_node;
            }
        }

        pub fn prepend(self: *SinglyLinkedList(T), allocator: std.mem.Allocator, value: T) !void {
            const new_head: *Node(T) = try allocator.create(Node(T));
            new_head.* = .{ .next = self.head, .value = try value.clone(allocator) };
            self.head = new_head;
        }

        pub fn clone(self: *const SinglyLinkedList(T), allocator: std.mem.Allocator) !SinglyLinkedList(T) {
            var new_list: SinglyLinkedList(T) = .{};
            if (self.head) |head| {
                new_list.head = try head.clone(allocator);
            }
            return new_list;
        }

        pub fn getIdxOf(self: *const SinglyLinkedList(T), search_for: *const T) ?usize {
            meta.requireDecl(T, "eql", fn (*const T, *const T) bool);
            meta.checkFunctionSignature(T, "eql", &.{ *const T, *const T }, bool, false);

            var current: ?*Node(T) = self.head;
            var i: usize = 0;
            while (current) |node| : (i += 1) {
                if (node.value.eql(search_for)) {
                    return i;
                }
                current = node.next;
            }
            return null;
        }

        pub fn getAt(self: *const SinglyLinkedList(T), idx: usize) ?*T {
            if (self.getNodeAt(idx)) |node| {
                return &node.value;
            }
            return null;
        }

        pub fn insertAt(self: *SinglyLinkedList(T), allocator: std.mem.Allocator, idx: usize, value: T) !void {
            if (idx == 0) {
                const new_head: *Node(T) = try allocator.create(Node(T));
                new_head.* = .{ .next = self.head, .value = try value.clone(allocator) };
                self.head = new_head;
                return;
            }
            var insert_after: *Node(T) = self.getNodeAt(idx - 1) orelse
                return error.IdxOutOfBounds;
            const inserted_ptr: *Node(T) = try allocator.create(Node(T));
            inserted_ptr.* = .{ .next = insert_after.next, .value = try value.clone(allocator) };
            insert_after.next = inserted_ptr;
        }

        pub fn removeAt(self: *SinglyLinkedList(T), allocator: std.mem.Allocator, idx: usize) error{IdxOutOfBounds}!void {
            if (idx == 0) {
                const old_head: *Node(T) = self.head orelse
                    return error.IdxOutOfBounds;
                self.head = old_head.next;
                old_head.deinit(allocator);
                allocator.destroy(old_head);
                return;
            }
            var remove_after: *Node(T) = self.getNodeAt(idx - 1) orelse
                return error.IdxOutOfBounds;
            const item_to_remove: *Node(T) = remove_after.next orelse
                return error.IdxOutOfBounds;
            const next: ?*Node(T) = item_to_remove.next;
            remove_after.next = next;
            item_to_remove.deinit(allocator);
            allocator.destroy(item_to_remove);
        }

        pub fn len(self: *const SinglyLinkedList(T)) usize {
            if (self.head) |head_value| {
                var length: usize = 0;
                var next: ?*Node(T) = head_value;
                while (next) |next_value| {
                    length += 1;
                    next = next_value.next;
                }
                return length;
            }
            return 0;
        }

        pub fn getNodeAt(self: *const SinglyLinkedList(T), idx: usize) ?*Node(T) {
            if (self.head) |head_value| {
                var length: usize = idx;
                var current: ?*Node(T) = head_value;
                while (length > 0) {
                    if (current == null) {
                        return null;
                    }
                    length -= 1;
                    current = current.?.next;
                }
                return current;
            }
            return null;
        }

        pub fn iter(self: *SinglyLinkedList(T)) Iterator {
            return .{ .current = self.head };
        }
    };
}

fn Node(comptime T: type) type {
    meta.requireDecl(T, "deinit", fn (*T, std.mem.Allocator) void);
    meta.requireDecl(T, "clone", fn (*const T, std.mem.Allocator) anyerror!T);

    meta.checkFunctionSignature(T, "deinit", &.{ *T, std.mem.Allocator }, void, false);
    meta.checkFunctionSignature(T, "clone", &.{ *const T, std.mem.Allocator }, T, true);

    return struct {
        next: ?*@This(),
        value: T,

        pub fn deinit(self: *Node(T), allocator: std.mem.Allocator) void {
            self.value.deinit(allocator);
        }

        pub fn clone(self: *const Node(T), allocator: std.mem.Allocator) !*Node(T) {
            const cloned_node: *Node(T) = try allocator.create(Node(T));
            if (self.next) |next| {
                cloned_node.next = try next.clone(allocator);
            } else {
                cloned_node.next = null;
            }
            cloned_node.value = try self.value.clone(allocator);
            return cloned_node;
        }

        pub fn clearAndFreeAll(self: *Node(T), allocator: std.mem.Allocator) void {
            if (self.next) |next| {
                next.clearAndFreeAll(allocator);
            }
            self.value.deinit(allocator);
            if (self.next) |next| {
                allocator.destroy(next);
                self.next = null;
            }
        }
    };
}
