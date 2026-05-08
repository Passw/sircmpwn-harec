// SPDX-License-Identifier: MPL-2.0
// (c) Hare authors <https://harelang.org>

let argc: size = 0;
let argv: *[*]*const u8 = null: *[*]*const u8;
let envp: *[*]nullable *const u8 = null: *[*]nullable *const u8;

// The real main function.
@symbol(".main") fn main() void;

// The purpose of this "fake" main function is to make sure we exit with the
// correct exit code in the case that rt::exit() is not called from within the
// program. The intialization and finalization functions are not run from here,
// they are ran by crt0.
export @symbol("main") fn _main(
	c_argc: int,
	c_argv: *[*]*const u8,
	c_envp: *[*]nullable *const u8
) void = {
	argc = c_argc: size;
	argv = c_argv;
	envp = c_envp;

	init();
	main();
	fini();
	exit(0);
};
