const std = @import("std");
const print = @import("std").debug.print;
const parseInt = @import("std").fmt.parseInt;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const buffer = try readFile("./input.txt", allocator);
    defer allocator.free(buffer);

    // Part 1
    var result: usize = 0;

    var line_it = std.mem.split(u8, buffer, "\n");
    while (line_it.next()) |line| {
        if (line.len == 0) continue;

        const firstDigitIndex = indexOf(std.ascii.isDigit, line);
        const lastDigitIndex = lastIndexOf(std.ascii.isDigit, line);

        if (firstDigitIndex == null or lastDigitIndex == null) {
            print("Line does not have two numbers: {s}\n", .{line});
            continue;
        }

        const numAsString = try std.fmt.allocPrint(allocator, "{c}{c}", .{ line[firstDigitIndex.?], line[lastDigitIndex.?] });
        result += try parseInt(usize, numAsString, 10);
    }

    // Part 2
    var sum2: usize = 0;

    line_it.reset();
    while (line_it.next()) |line| {
        if (line.len == 0) continue;

        const firstDigitIndex = indexOf(std.ascii.isDigit, line);
        const lastDigitIndex = lastIndexOf(std.ascii.isDigit, line);
        const firstDigitStr = findNum(line);
        const lastDigitStr = findLastNum(line);

        var firstDigitStrIndex: ?usize = null;
        if (firstDigitStr) |firstDigitStrOK| firstDigitStrIndex = firstDigitStrOK.index;

        var lastDigitStrIndex: ?usize = null;
        if (lastDigitStr) |lastDigitStrOK| lastDigitStrIndex = lastDigitStrOK.index;

        const firstIndex = min(firstDigitIndex, firstDigitStrIndex);
        const lastIndex = max(lastDigitIndex, lastDigitStrIndex);

        if (firstIndex == null or lastIndex == null) {
            print("Line does not have two numbers: {s}\n", .{line});
            continue;
        }

        var digit1: usize = undefined;
        if (firstIndex.? == firstDigitIndex) {
            digit1 = try std.fmt.charToDigit(line[firstIndex.?], 10);
        } else {
            digit1 = firstDigitStr.?.value;
        }

        var digit2: usize = undefined;
        if (lastIndex.? == lastDigitIndex) {
            digit2 = try std.fmt.charToDigit(line[lastIndex.?], 10);
        } else {
            digit2 = lastDigitStr.?.value;
        }

        const calibration_value_str = try joinDigits(digit1, digit2, allocator);
        defer allocator.free(calibration_value_str);

        const calibration_value = try parseInt(usize, calibration_value_str, 10);
        sum2 += calibration_value;
    }

    print("Part 1: {d}\n", .{result});
    print("Part 2: {d}\n", .{sum2});
}

fn readFile(path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    const stat = try file.stat();
    const file_size = stat.size;

    const buffer = try file.readToEndAlloc(allocator, file_size);
    return buffer;
}

const NumLocationAndValue = struct {
    index: usize,
    value: usize,
};

fn findNum(str: []const u8) ?NumLocationAndValue {
    const numbers = [_][]const u8{ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

    var first: ?NumLocationAndValue = null;

    for (numbers, 0..) |num, i| {
        if (std.mem.indexOf(u8, str, num)) |startIndex| {
            if (first) |firstOK| {
                if (firstOK.index > startIndex) first = .{ .index = startIndex, .value = i + 1 };
            } else {
                first = .{ .index = startIndex, .value = i + 1 };
            }
        }
    }

    return first;
}

fn findLastNum(str: []const u8) ?NumLocationAndValue {
    const numbers = [_][]const u8{ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

    var last: ?NumLocationAndValue = null;

    for (numbers, 0..) |num, i| {
        if (std.mem.lastIndexOf(u8, str, num)) |startIndex| {
            if (last) |lastOK| {
                if (lastOK.index < startIndex) last = .{ .index = startIndex, .value = i + 1 };
            } else {
                last = .{ .index = startIndex, .value = i + 1 };
            }
        }
    }

    return last;
}

fn joinDigits(digit1: usize, digit2: usize, allocator: std.mem.Allocator) ![]u8 {
    return try std.fmt.allocPrint(allocator, "{d}{d}", .{ digit1, digit2 });
}

fn min(a: ?usize, b: ?usize) ?usize {
    if (a == null and b == null) return null;
    const items = [2]usize{ a orelse b.?, b orelse a.? };
    return std.mem.min(usize, &items);
}

fn max(a: ?usize, b: ?usize) ?usize {
    if (a == null and b == null) return null;
    const items = [2]usize{ a orelse b.?, b orelse a.? };
    return std.mem.max(usize, &items);
}

fn indexOf(testFn: *const fn (c: u8) bool, in: []const u8) ?usize {
    for (in, 0..) |character, index| {
        if (testFn(character)) return index;
    }
    return null;
}

fn lastIndexOf(testFn: *const fn (c: u8) bool, in: []const u8) ?usize {
    var i = in.len;
    while (i > 0) {
        i -= 1;
        if (testFn(in[i])) {
            return i;
        }
    }
    return null;
}
