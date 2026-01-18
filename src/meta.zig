const std = @import("std");

pub fn requireField(
    comptime T: type,
    comptime name: []const u8,
    comptime ExpectedType: type,
) void {
    comptime if (!@hasField(T, name)) {
        @compileError(std.fmt.comptimePrint(
            "Struct {s} has no field named \"{s}\"",
            .{ @typeName(T), name },
        ));
    };

    const ActualType = @FieldType(T, name);
    if (ActualType != ExpectedType) {
        @compileError(std.fmt.comptimePrint(
            "Field `{s}.{s}` has wrong type. Expected `{s}`, found `{s}`",
            .{ @typeName(T), name, @typeName(ExpectedType), @typeName(ActualType) },
        ));
    }
}

pub fn requireDecl(
    comptime T: type,
    comptime name: []const u8,
    comptime ExpectedType: type,
) void {
    comptime if (!@hasDecl(T, name)) {
        @compileError(std.fmt.comptimePrint(
            "Struct `{s}` has no declaration named `{s}`",
            .{ @typeName(T), name },
        ));
    };
    const ActualType = @TypeOf(@field(T, name));
    if (ExpectedType != ActualType) {
        @compileError(std.fmt.comptimePrint(
            "Declaration `{s}.{s}` has wrong type: expected `{s}`, found `{s}`",
            .{ @typeName(T), name, @typeName(ExpectedType), @typeName(ActualType) },
        ));
    }
}

pub fn checkFunctionSignature(
    comptime T: type,
    comptime function_name: []const u8,
    comptime ExpectedParamTypes: []const type,
    comptime ExpectedReturnType: type,
    comptime throws: bool,
) void {
    // Print expected function signature if declaration does not exist
    if (!@hasDecl(T, function_name)) {
        comptime var param_list_str: []const u8 = "";
        comptime {
            for (0..ExpectedReturnType.len) |i| {
                if (i > 0) {
                    param_list_str = std.fmt.comptimePrint("{s}, ", .{param_list_str});
                }
                param_list_str = std.fmt.comptimePrint("{s}{s}", .{ param_list_str, @typeName(ExpectedReturnType[i]) });
            }
        }
        @compileError("Type " ++ @typeName(T) ++ " must implement: " ++
            function_name ++ "(" ++ param_list_str ++ ") " ++
            (if (throws) "!" else "") ++ @typeName(ExpectedReturnType));
    }

    // Validate parameter count
    const fn_type: std.builtin.Type = @typeInfo(@TypeOf(@field(T, function_name)));
    const fn_info: std.builtin.Type.Fn = fn_type.@"fn";
    if (fn_info.params.len != ExpectedParamTypes.len) {
        @compileError(std.fmt.comptimePrint("Function {s} must have {d} parameters", .{ function_name, ExpectedParamTypes.len }));
    }

    // Check parameter types
    inline for (0..ExpectedParamTypes.len) |i| {
        const param_type = fn_info.params[i].type.?;
        const expected_param_type = ExpectedParamTypes[i];

        if (param_type != expected_param_type) {
            @compileError("Parameter " ++ std.fmt.comptimePrint("{d}", .{i}) ++
                " must be " ++ @typeName(expected_param_type) ++
                ", but found " ++ @typeName(param_type));
        }
    }

    // Return type and error handling validation
    const actual_return_type = fn_info.return_type.?;
    const is_error_union: bool = @typeInfo(actual_return_type) == .error_union;

    if (is_error_union != throws) {
        @compileError(std.fmt.comptimePrint("Function {s} error handling mismatch: {s} expected", .{ function_name, if (throws) "error union" else "non-error union" }));
    }

    const base_return_type = switch (@typeInfo(actual_return_type)) {
        .error_union => |error_union| error_union.payload,
        else => actual_return_type,
    };

    if (base_return_type != ExpectedReturnType) {
        @compileError(std.fmt.comptimePrint("Return type mismatch for {s}: expected {s}, got {s}", .{ function_name, @typeName(ExpectedReturnType), @typeName(base_return_type) }));
    }
}
