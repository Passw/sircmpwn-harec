use rt;

type coords = struct { x: i8, y: int, z: size };
type anyint = struct { _8: i8, _16: i16, _32: i32, _64: i64 };

export fn main() void = {
	// Simple case
	let x = 10;
	let y = x;
	assert(y == 10);

	// With indirect target
	let a = [1, 2, 3, 4];
	let x = 2z;
	assert(a[x] == 3);

	// Aggregate types:
	// arrays
	let x = [1, 2, 3, 4];
	let y = x;
	assert(&x != &y);
	assert(x[0] == y[0]);
	assert(x[1] == y[1]);
	assert(x[2] == y[2]);
	assert(x[3] == y[3]);

	// structs
	let a = coords { x = 10, y = 20, z = 30 };
	let b = a;
	assert(&a != &b);
	assert(a.x == b.x);
	assert(a.y == b.y);
	assert(a.z == b.z);

	// unions
	let a = anyint { _16 = 10, ... };
	let b = a;
	assert(&a != &b);
	assert(a._16 == b._16);

	// tuples
	let a = (1, 2z, 3u8);
	let b = a;
	assert(a.0 == b.0);
	assert(a.1 == b.1);
	assert(a.2 == b.2);

	let x = "hello world";
	let y = x;
	let px = &x: *struct {
		data: *[*]u8,
		length: size,
		capacity: size,
	};
	let py = &y: *struct {
		data: *[*]u8,
		length: size,
		capacity: size,
	};
	assert(px.length == py.length);
	assert(px.capacity == py.capacity);
	assert(px.data == py.data);

	let x: []int = [1, 2, 3, 4];
	let y = x;
	let px = &x: *rt::slice;
	let py = &y: *rt::slice;
	assert(px.data == py.data);
	assert(px.length == py.length);
	assert(px.capacity == py.capacity);
};
