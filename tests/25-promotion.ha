use rt::{compile, status};

export fn main() void = {
	assert(0xFFu8 << 8u16 == 0xFF00u16);
	assert(0xFFu8 << 8u8 == 0u8);
	assert(0xFFu8 << 24u32 == 0xFF000000u32);
	assert(0xFFu8 << 32u32 == 0xFFu32);
	assert(0xFFu8 << 32u64 == 0xFF00000000u64);
	assert(0xFFu8 << 64u64 == 0xFFu64);

	assert(300u8 < 200u16);
	assert(300u8: f64 == 44.0);

	static assert(300u8 < 200u16);
	static assert(300u8: f64 == 44.0);

	static assert(size(size) == 8); // XXX
	static assert(size(int) == 4); // XXX
	let failures = [
		"let a = 0z + 0u64;",
		"let a = 0i + 0i32;",
		"let a = 0u + 0u32;",
	];
	for (let f .. failures) {
		compile(status::CHECK, f)!;
	};
};
