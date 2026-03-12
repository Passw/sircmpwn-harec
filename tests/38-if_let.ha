use rt;
use rt::{compile, status};

fn tagged() void = {
	let x: (int | uint) = 10u;
	if (let q: uint = x) {
		assert(q == 10);
	} else {
		abort();
	};

	if (let q: *uint = &x) {
		assert(*q == 10);
	} else {
		abort();
	};

	let success = false;
	if (let q: int = x) {
		abort();
	} else {
		success = true;
	};
	assert(success);

	if (let q: uint = x) {
		assert(q == 10);
	} else if (let q: int = x) {
		abort();
	};
};

fn pointers() void = {
	let x: int = 10;
	let y: nullable *int = &x;

	if (let q: *int = y) {
		assert(*q == 10);
	} else {
		abort();
	};
};

fn results() void = {
	let x: (int | uint) = 10u;

	const b = if (let q: uint = x) {
		yield q;
	} else abort();

	assert(b == 10u);

	let x: (int | uint | f64) = 42u;
	const b = if (let q: uint = x) {
		yield q * 2;
	};
	assert(b is uint && !(b is void));
	assert(b as uint == 84);
};

fn errors() void = {
	// Binding scoped correctly
	compile(status::CHECK, "
	fn a() void = {
		let x: (int | uint) = 10u;
		if (let q: uint = x) {
			void;
		} else {
			q;
		};
	};
	")!;

	// Must provide a type
	compile(status::CHECK, "
	fn a() void = {
		let x: (int | uint) = 10u;
		if (let q = x) {
			void;
		};
	};
	")!;

	// Type must be compatible
	compile(status::CHECK, "
	fn a() void = {
		let x: (int | uint) = 10u;
		if (let q: f64 = x) {
			void;
		};
	};
	")!;
};

export fn main() void = {
	tagged();
	pointers();
	results();
	errors();
};
