// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// bitfuckit FFI Build Configuration (Zig 0.15.2+, verified against 0.16.0)
// Note: This is a minimal build file — it type-checks the FFI sources via
// `zig fmt`/`zig test` invocations from the Justfile. No library artifact is
// produced yet (see Justfile `build` recipe).

const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.standardTargetOptions(.{});
    _ = b.standardOptimizeOption(.{});

    // In Zig 0.15+, tests are run directly with:
    //   zig build-exe -ftest-runner src/main.zig
    //   zig build-exe -ftest-runner test/integration_test.zig
    //
    // This minimal build file provides scaffolding for future expansion.
    // Tests can be invoked via command line without explicit build.zig configuration.
}
