@symbol("main") fn main() void;

let argc: size = 0;
let argv: *[*]*const u8 = null: *[*]*const u8;
let envp: *[*]nullable *const u8 = null: *[*]nullable *const u8;

export fn start_ha(iv: *[*]uintptr) never = {
	argc = iv[0]: size;
	argv = &iv[1]: *[*]*const u8;
	envp = &argv[argc + 1]: *[*]nullable *const u8;

	init();
	main();
	fini();
	exit(0);
};
