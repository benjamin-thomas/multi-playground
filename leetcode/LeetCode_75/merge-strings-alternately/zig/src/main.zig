// echo ./src/main.zig | entr -c bash -c 'zig build test && echo OK'

const std = @import("std");
const testing = std.testing;

fn greet(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    const slices = [_][]const u8{ "AAA", name, "CCC"};
    return try std.mem.join(allocator, "-", &slices);
}

test "joining strings" {
    const allocator = std.testing.allocator;
    const greeting = try greet(allocator, "BBB");
    defer allocator.free(greeting);
    const okay = std.mem.eql(u8, "AAA-BBB-CCC", greeting);
    try testing.expect(okay);
}

fn interleave(allocator: std.mem.Allocator, a: []const u8, b: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, a.len + b.len);
    var i: usize = 0;
    var j: usize = 0;

   while (i < a.len or j < b.len) {
       if (i < a.len) {
           result[j] = a[i];
           j+=1;
       }
       if (i < b.len) {
           result[j] = b[i];
           j+=1;
       }
       i += 1;

   }
   return result;
}

test "interleaving strings" {
    const allocator = std.testing.allocator;

    const word = try interleave(allocator, "AC", "BD");
    defer allocator.free(word);
    try testing.expect(std.mem.eql(u8, "ABCD", word));

    // Below crashes, I'm stopping this Zig experiment for now.
    // const word2 = try interleave(allocator, "AB_X", "CD");
    // defer allocator.free(word2);
    // try testing.expect(std.mem.eql(u8, "ABCD_X", word2));


}