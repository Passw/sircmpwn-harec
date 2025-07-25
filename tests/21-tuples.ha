use rt::{compile, status};

def CONST1: (int, str) = (15, "foo");
def CONST2: [_](int, str) = [(15, "foo"), (30, "bar")];

fn storage() void = {
	let x: (int, size) = (42, 1337);
	assert(size((int, size)) == size(size) * 2);
	assert(size((int, (u8, size))) == size(size) * 3);
	assert(align((int, size)) == align(size));
	assert(align((int, (u8, size))) == align(size));
	let ptr = &x: *struct { i: int, z: size };
	assert(ptr.i == 42 && ptr.z == 1337);

	assert(size((i8, int, size)) == 2 * size(size));
	assert(size((int, i8, size)) == 2 * size(size));
	assert(size((size, int, i8)) == 2 * size(size));
	assert(size((size, i8, int)) == 2 * size(size));
	assert(size((i8, size, int)) == 3 * size(size));
	assert(size((int, size, i8)) == 3 * size(size));

	let x: (int, void, size) = (42, void, 1337);
	assert(size((int, void, size)) == size((int, size)));
	assert(size((int, (void, size))) == size((int, size)));
	assert(align((int, void, size)) == align(size));
	assert(align((int, (void, size))) == align(size));
	let ptr = &x: *struct { i: int, z: size };
	assert(ptr.i == 42 && ptr.z == 1337);

	let x: (void, void) = (void, void);
	assert(size((void, void)) == 0);
};

fn indexing() void = {
	let x: ((int, uint), size) = ((42, 69), 1337);
	assert(x.0.0 == 42);
	assert(x.0.1 == 69);
	assert(x.1 == 1337);
	assert(x.1z == 1337);
	assert(x.0x1 == 1337);
	assert(x.1e+0 == 1337);
};

fn assignment() void = {
	let x = (42, 1337z);
	assert(x.0 == 42 && x.1 == 1337);
	x.0 = 1337;
	x.1 = 42;
	assert(x.0 == 1337 && x.1 == 42);

	let x = (void, void);
	x.0 = x.1;
	x.1 = void;
};

fn func(in: (int, size)) (int, size) = (in.0 + 1, in.1 + 1);
fn eval_expr_access() void = {
	static assert((42, 0).0 == 42 && (42, 0).1 == 0);
	static assert(CONST1.0 == 15 && CONST1.1 == "foo");
	static assert(CONST2[0].0 == 15 && CONST2[0].1 == "foo");
	static assert(CONST2[1].0 == 30 && CONST2[1].1 == "bar");
};

fn eval_expr_tuple() void = {
	static let t = (42, 8);
};

fn funcs() void = {
	let x = func((41, 1336));
	assert(x.0 == 42 && x.1 == 1337);
};

fn unpacking_static() int = {
	static let (a, b) = (0, 0);
	a += 1;
	b += 1;
	return a;
};

fn unpacking_demo() (int, int) = {
	return (10, 20);
};

fn unpacking_eval() (int, int) = {
	static let i = 0;
	const res = (10 + i, 20 + i);
	i += 1;
	return res;
};

let unpacking_global: int = 0i;

fn unpacking_addone() int = {
	unpacking_global += 1;
	return unpacking_global;
};

type tuple_alias = (int, int);
fn unpacking() void = {
	const (a, b, c) = (42, 8, 12);
	assert(a == 42);
	assert(b == 8);
	assert(c == 12);

	const (a, b): (i64, u64) = (2i, 4z);
	assert(a == 2i64);
	assert(b == 4u64);

	const (a, b, c): (i64, str, f64) = (2i, "hello", 1.0);
	assert(a == 2i64);
	assert(b == "hello");
	assert(c == 1.0f64);

	let (a, b): (i64, u64) = (1i, 3z);
	a += 1;
	b += 1;
	assert(a == 2i64);
	assert(b == 4u64);

	const (_, b, c) = (1, 2, 3);
	assert(b == 2);
	assert(c == 3);

	const (a, _, c) = (1, 2, 3);
	assert(a == 1);
	assert(c == 3);

	const (a, b, _) = (1, 2, 3);
	assert(a == 1);
	assert(b == 2);

	const t: tuple_alias = (1, 2);
	const (a, b) = t;
	assert(a == 1);
	assert(b == 2);

	unpacking_static();
	unpacking_static();
	const a = unpacking_static();
	assert(a == 3);

	const (a, b) = unpacking_demo();
	assert(a == 10);
	assert(b == 20);

	const (a, b) = unpacking_eval();
	assert(a == 10);
	assert(b == 20);

	let (a, b, _, d) = (unpacking_addone(), unpacking_addone(),
		unpacking_addone(), unpacking_addone());
	assert(a == 1 && b == 2 && d == 4);
};

fn _offset() void = {
	const x = (0i32, void, 0i64);
	static assert(offset(x.0) == 0);
	static assert(offset(x.1) == 4);
	static assert(offset(x.2) == align(i64));
};

// Regression tests for miscellaneous compiler bugs
fn regression() void = {
	let a: (((int | void), int) | void) = (void, 0);

	let x = (1, 0);
	x = (2, x.0);
	assert(x.0 == 2 && x.1 == 1);
};

fn reject() void = {
	let parse = [
		// unpack with def
		"fn t() void = { def (x, y) = (1, 2); };",
		// empty tuple
		"fn t() void = { let a: (() | void) = void; };",
		"fn t() void = { let a = (); };",
		"fn t() void = { let () = (); };",
		// one member
		"fn t() void = { let a: ((int) | void) = void; };",
		// null type
		"fn t() void = { let a: ((null, int) | void) = void; };",
		// invalid field access
		"fn t() void = { let a = (0, 1, 2); a.-1; };",
		"fn t() void = { let a = (0, 1, 2).-1; };",
	];
	let check = [
		// no name in unpack
		"fn t() void = { let (_, _) = (1, 2); };",
		// unpack of non-tuple type
		"fn t() void = { let (x, y) = 5; };",
		"fn t() void = { let (a, b): int = (2, 3); };",
		// static unpack
		"fn getval() int = 5; fn t() void = { static let (a, b) = (2, getval()); };",
		"fn getval() int = 5; fn t() void = { static let (a, _) = (2, getval()); };",

		// member count mismatch
		"fn t() void = { let a: (u8, u8, u8) = (2, 3); };",
		"fn t() void = { let a: (u8, u8) = (2, 3); let b: (u8, u8, u8) = a; };",
		"fn t() void = { let a: (u8, u8) = (2, 3, 4); };",
		"fn t() void = { let a: (u8, u8, u8) = (2, 3, 4); let b: (u8, u8) = a; };",
		"fn t() void = { let (x, y) = (1, 2, 3); };",
		"fn t() void = { let (x, y, z) = (1, 2); };",
		"fn t() void = { let (x, y, _) = (1, 2); };",
		"fn t() void = { let (x, _, _) = (1, 2); };",
		// one member
		"fn t() void = { let a = (4u); a.0; };",
		"fn t() void = { let (a) = (4u); a.0; };",
		"fn t() void = { let (a) = 4u; a.0; };",
		// member of undefined size
		"fn t() void = { let a: (([*]int, int) | void) = void; };",
		"fn t() void = { let a = (t, 2); };",
		"fn t() void = { let (a, b) = (t, 2); };",
		"fn t() void = { let (_, b) = (t, 2); };",
		// null type member
		"fn t() void = { let a = (null, 2); };",
		"fn t() void = { let (a, b) = (null, 2); };",
		"fn t() void = { let (_, b) = (null, 2); };",
		// invalid field access
		"fn t() void = { let a = (0, 1, 2); a.3; };",
		"fn t() void = { let a = (0, 1, 2).3; };",

		// arithmetic on tuples
		"fn t() void = { let a = (0, 1) + (2, 3); };",
		"fn t() void = { let a = (0, 1); a += (2, 3); };",
	];

	for (let i = 0z; i < len(parse); i += 1) {
		compile(status::PARSE, parse[i])!;
	};
	for (let i = 0z; i < len(check); i += 1) {
		compile(status::CHECK, check[i])!;
	};
};

fn _never() void = {
	{ let x: (int, int) = (yield, 1); };
	{ let (y, z): (int, int) = (yield, 1); };
};

export fn main() void = {
	storage();
	indexing();
	assignment();
	funcs();
	eval_expr_tuple();
	eval_expr_access();
	unpacking();
	_offset();
	regression();
	reject();
	_never();
};
