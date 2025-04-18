use rt::{compile, status};

type slice = struct {
	data: nullable *opaque,
	length: size,
	capacity: size,
};

fn from_array() void = {
	let src = [1, 2, 3];
	let x: []int = src;
	let xptr = &x: *slice;
	assert(xptr.data == &src);

	let y: []int = [];
	let yptr = &y: *slice;
	assert(yptr.data == null);
};

fn storage() void = {
	let x: []int = [1, 2, 3, 4, 5];
	const expected = [1, 2, 3, 4, 5];

	let ptr = &x: *slice;

	assert(len(x) == 5);
	assert(ptr.length == 5 && ptr.capacity == 5);

	for (let i = 0z; i < len(expected); i += 1) {
		assert(x[i] == expected[i]);
	};

	let x: *[1]u8 = alloc([0...])!;
	free(x);
};

fn casting() void = {
	let x: []int = [1, 2, 3, 4, 5];
	let y = x: *[5]int;
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == y[i]);
	};

	[]: []int: []opaque;

	compile(status::CHECK,
		"fn test() void = { []: []opaque; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { [1]: []opaque; };"
	)!;
};

fn measurements() void = {
	let x: []int = [1, 2, 3, 4, 5];
	assert(size([]int) == size(*[*]int) + size(size) * 2);
	assert(align([]int) == (if (align(*[*]int) > align(size)) align(*[*]int)
		else align(size)));
	assert(len(x) == 5);
	assert(&x[0]: uintptr: size % size(int) == 0);
	static assert(len([1, 2, 3, 4, 5]: []int) == 5);

	compile(status::CHECK,
		"fn test() void = { let x = []; static assert(len(x) == 0); };"
	)!;
};

fn indexing() void = {
	let x = [1, 3, 3, 7];
	assert(x[0] == 1 && x[1] == 3 && x[2] == 3 && x[3] == 7);
	compile(status::CHECK,
		"fn test() void = { let x: []int = [1, 2, 3]; x[\"hello\"]; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { let x = 10; x[10]; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { let s: []u8 = []; let ss = s: []opaque; ss[0] = ss[1]; };"
	)!;
};

fn zero3(s: []int) void = {
	s[..] = [0, 0, 0];
};

type sl_alias = []int;

fn assignment() void = {
	let source = [1, 2, 3];
	let x: []int = source;
	x[0] = 4;
	x[1] = 5;
	x[2] = 6;
	assert(x[0] == 4 && x[1] == 5 && x[2] == 6);
	assert(source[0] == 4 && source[1] == 5 && source[2] == 6);
	let y: []int = [4, 5, 6];
	x = y;
	x[0] = 7;
	x[1] = 8;
	x[2] = 9;
	assert(x[0] == 7 && x[1] == 8 && x[2] == 9);
	assert(source[0] == 4 && source[1] == 5 && source[2] == 6);

	zero3(y);
	assert(y[0] == 0 && y[1] == 0 && y[2] == 0);
	let z: []int = [1, 2, 3, 4, 5];
	z[1..4] = [42, 69, 1337];
	assert(z[0] == 1 && z[1] == 42 && z[2] == 69 && z[3] == 1337 && z[4] == 5);
	z[2..5] = y;
	assert(z[0] == 1 && z[1] == 42 && z[2] == 0 && z[3] == 0 && z[4] == 0);
	let z: sl_alias = z;
	z[2..5] = y;
	let z: *sl_alias = &z;
	z[2..5] = y;

	let x: []int = [];
	let opaqueslice: []opaque = x;
	let opaqueslice: []opaque = []: []int;

	compile(status::CHECK,
		"export fn main() void = { let a: []int = [1]; a[..] += a; };"
	)!;
	compile(status::CHECK,
		"fn f() void = { let a: []int = [1] + [2]; };"
	)!;
	compile(status::CHECK,
		"type t = opaque; fn f() void = { let x: []int = []; let x: []t = x; };"
	)!;
};

fn assert_slice_eq(actual: []int, expected: []int) void = {
	assert(len(expected) == len(actual));
	for (let i = 0z; i < len(expected); i += 1) {
		assert(expected[i] == actual[i]);
	};
};

fn cap(s: []int) size = (&s: *slice).capacity;

fn slicing() void = {
	let a: [_]int = [1, 2, 3, 4, 5];
	assert_slice_eq(a[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(a[..3], [1, 2, 3]);
	assert_slice_eq(a[1..3], [2, 3]);
	assert_slice_eq(a[1..], [2, 3, 4, 5]);
	assert_slice_eq(a[5..], []);

	assert(cap(a[..0]) == len(a));
	assert(cap(a[..2]) == len(a));
	assert(cap(a[..]) == len(a));
	assert(cap(a[2..]) == len(a) - 2);
	assert(cap(a[5..]) == 0);

	let b = a[..3];
	assert(cap(b[..0]) == len(a));
	assert(cap(b[..2]) == len(a));
	assert(cap(b[..]) == len(a));
	assert(cap(b[2..]) == len(a) - 2);
	assert(cap(b[3..]) == 2);

	let b: []int = [1, 2, 3, 4, 5];
	assert_slice_eq(b[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(b[..3], [1, 2, 3]);
	assert_slice_eq(b[1..3], [2, 3]);
	assert_slice_eq(b[1..], [2, 3, 4, 5]);
	assert_slice_eq(b[5..], []);

	let p = &a;
	assert_slice_eq(p[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(p[..3], [1, 2, 3]);
	assert_slice_eq(p[1..3], [2, 3]);
	assert_slice_eq(p[1..], [2, 3, 4, 5]);
	assert_slice_eq(p[5..], []);

	let check_failures = [
		"fn test() void = { let x = \"test\"; x[1..3]; };",
		"fn test() void = { let x = 0; x[1..3]; };",
		"fn test() void = { let x = [1, 2, 3]; x[\"hi\"..]; };",
		"fn test() void = { let x = [1, 2, 3]; x[false..]; };",
		"fn test() void = { let x = [1, 2, 3]; x[..false]; };",
		"fn test() void = { let x = [1, 2, 3]; x[..\"hi\"]; };",
		"fn test() void = { let x = [1, 2, 3]; x[2..1]; };",
		"fn test() void = { let x = [1, 2, 3]; x[..4]; };",
		"fn test() void = { let x = [1, 2, 3]; x[..4]; };",

		"fn test(a: *[*]u8) void = { a[..]; };",
		"fn test(a: *[*]u8) void = { a[2..]; };",
		"fn test(opa: *[*]opaque) void = { opa[..]; };",
		"type op = opaque; fn test(opa: *[*]op) void = { opa[..]; };",

		"fn test(opsl: []opaque) void = { opsl[1..]; };",
		"type op = opaque; fn test(sl: []op) void = { sl[1..]; };",
		"fn test(opa: *[*]opaque) void = { opa[1..]; };",
		"fn test(opa: *[*]opaque) void = { opa[1..1]; };",
		"type op = opaque; fn test(opa: *[*]op) void = { opa[1..]; };",
		"type op = opaque; fn test(opa: *[*]op) void = { opa[1..1]; };",
	];
	for (let tc .. check_failures) {
		compile(status::CHECK, tc)!;
	};
};

type tree = struct {
	value: u64,
	children: []tree,
};

fn sum_tree(t: tree) u64 = {
	let sum = t.value;
	for (let i = 0z; i < len(t.children); i += 1) {
		sum += sum_tree(t.children[i]);
	};
	return sum;
};

fn recursive_structure() void = {
	const t = tree {
		value = 15,
		children = [tree {
			value = 23,
			children = [tree {
				value = 62,
				children = [],
			}, tree {
				value = 34,
				children = [],
			}],
		}],
	};
	assert(sum_tree(t) == 134, "recursive structure using slices");
};

fn expandable() void = {
	let s: [6]u64 = [0...];

	s[1..3] = [1...];
	assert(s[0] == 0);
	assert(s[1] == 1);
	assert(s[2] == 1);
	assert(s[3] == 0);

	s[2..] = [123...];
	assert(s[1] == 1);
	assert(s[2] == 123);
	assert(s[3] == 123);
	assert(s[4] == 123);
	assert(s[5] == 123);
};

fn misc_reject() void = {
	// can't have slice of void
	compile(status::CHECK,
		"fn test() void = { let x: []void = [12]; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { let x = [void]: []opaque; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { []: []void; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { let x: ([]void | void) = void; };"
	)!;
	compile(status::CHECK,
		"fn test() void = { []: []never; };"
	)!;
};

fn lencap(s: []int) (size, size) = {
	let ptr = &s: *slice;
	return (ptr.length, ptr.capacity);
};

fn cap_borrowed() void = {
	// size defined, cap = size - L
	let b: [42]int = [0...];
	let (length, capacity) = lencap(b[20..25]);
	assert(length == 5);
	assert(capacity == 22);

	// no size defined, cap = H - L
	let b2: *[*]int = &b;
	let (length, capacity) = lencap(b2[20..25]);
	assert(length == 5);
	assert(capacity == 5);
};

type s = struct {
	i: int,
	data: [4]int,
	tuple: ([2]int, int),
	nested: [4][3]int,
};
let global = s { ... };

let backing = [1, 2, 3, 4];
let ref = backing[1..3];
let ref_chained = backing[1..][..2];

let literal = [1, 2, 3][..2];

let chained = [1, 2, 3, 4][1..][..2];
let long_chain = [1, 2, 3, 4][1..][1..][1..];

fn eval() void = {
	static assert(len([1, 2, 3][1..2]) == 1);
	static assert(len([1, 2, 3][1..][..1]) == 1);
	static assert(len(backing[1..2]) == 1);
	static assert(len(backing[1..2][..]) == 1);

	let (length, capacity) = lencap(literal);
	assert(length == 2);
	assert(capacity == 3);
	assert(literal[0] == 1 && literal[1] == 2);

	let (length, capacity) = lencap(chained);
	assert(length == 2);
	assert(capacity == 3);
	assert(chained[0] == 2 && chained[1] == 3);

	let (length, capacity) = lencap(ref);
	assert(length == 2);
	assert(capacity == 3);
	assert(ref[0] == 2 && ref[1] == 3);

	let (length, capacity) = lencap(ref_chained);
	assert(length == 2);
	assert(capacity == 3);
	assert(ref[0] == 2 && ref[1] == 3);

	let (length, capacity) = lencap(global.data[..0]);
	assert(length == 0);
	assert(capacity == 4);

	let (length, capacity) = lencap(global.tuple.0[1..]);
	assert(length == 1);
	assert(capacity == 1);

	let (length, capacity) = lencap(global.tuple.0[..][1..]);
	assert(length == 1);
	assert(capacity == 1);

	let (length, capacity) = lencap(global.nested[2][1..]);
	assert(length == 2);
	assert(capacity == 2);

	let (length, capacity) = lencap(global.nested[2][1..][..1]);
	assert(length == 1);
	assert(capacity == 2);

	compile(status::CHECK, "let x = [1, 2, 3][2..1];")!;
	compile(status::CHECK, "let x = [1, 2, 3][..10];")!;
	compile(status::CHECK, "let x = [1, 2, 3][4..];")!;

	compile(status::CHECK, "let x = [1, 2, 3][..][2..1];")!;
	compile(status::CHECK, "let x = [1, 2, 3][..][..10];")!;
	compile(status::CHECK, "let x = [1, 2, 3][..][4..];")!;

	compile(status::CHECK, "let x = [1, 2, 3]; let y = x[2..1];")!;
	compile(status::CHECK, "let x = [1, 2, 3]; let y = x[..10];")!;
	compile(status::CHECK, "let x = [1, 2, 3]; let y = x[4..];")!;

	compile(status::CHECK, "let x = [1, 2, 3]; let y = x[..]; let a = y[..];")!;
	compile(status::CHECK, "let x = [1, 2, 3]; let y = x[..]; let a = y[1];")!;
};

export fn main() void = {
	from_array();
	storage();
	measurements();
	indexing();
	assignment();
	slicing();
	recursive_structure();
	expandable();
	misc_reject();
	cap_borrowed();
	eval();
};
