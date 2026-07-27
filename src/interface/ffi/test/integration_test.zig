// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// bitfuckit FFI Integration Tests
//
// These tests are meant to verify that the Zig FFI (src/main.zig) correctly
// implements the Idris2 ABI (src/interface/Abi/*.idr) when compiled and
// linked as a standalone library. The unit tests inside src/main.zig already
// cover the same lifecycle/error/version behaviour in-process; this file is
// the separate-compilation-unit variant, not yet wired into build.zig as a
// distinct test target (see build.zig).
//
// To activate: declare `extern fn bitfuckit_init() ?*Handle;` etc. matching
// the `export fn` signatures in src/main.zig, define `const Handle = opaque {};`,
// and uncomment the example tests below once build.zig links the two files.

const std = @import("std");

// extern fn bitfuckit_init() ?*Handle;
// extern fn bitfuckit_free(?*Handle) void;
// const Handle = opaque {};

test "placeholder test - implementation required" {
    // This test ensures the file compiles
    // Actual tests depend on FFI implementation
    try std.testing.expect(true);
}

// ==============================================================================
// Example tests (uncomment once build.zig links this file against main.zig):
// ==============================================================================
//
// test "lifecycle: create and destroy handle" {
//     const handle = bitfuckit_init() orelse return error.InitFailed;
//     defer bitfuckit_free(handle);
// }
//
// test "operations: process with valid handle" {
//     const handle = bitfuckit_init() orelse return error.InitFailed;
//     defer bitfuckit_free(handle);
//
//     const result = bitfuckit_process(handle, 42);
//     try std.testing.expectEqual(@as(c_int, 0), result);
// }
//
// test "memory safety: double free is safe" {
//     const handle = bitfuckit_init() orelse return error.InitFailed;
//     bitfuckit_free(handle);
//     bitfuckit_free(handle); // Should not crash
// }
//
// test "strings: get string result from handle" {
//     const handle = bitfuckit_init() orelse return error.InitFailed;
//     defer bitfuckit_free(handle);
//
//     const str = bitfuckit_get_string(handle);
//     defer if (str) |s| bitfuckit_free_string(s);
//
//     try std.testing.expect(str != null);
// }
//
// test "version: returns non-empty version string" {
//     const ver = bitfuckit_version();
//     const ver_str = std.mem.span(ver);
//     try std.testing.expect(ver_str.len > 0);
// }
