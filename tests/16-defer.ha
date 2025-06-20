use rt;
use rt::{compile, status};

let x: int = 10;

fn basics() void = {
	assert(x == 10);
	defer x = 20;
	assert(x == 10);
	if (true) {
		return;
	};
	defer x = 30;
};

fn scope() void = {
	let x = 10;
	{
		defer x = 20;
		assert(x == 10);
	};
	assert(x == 20);
};

fn loops() void = {
	let x = 0;
	for (let i = 0; i < 5; i += 1) {
		defer x += 1;
		assert(x == i);
	};
	assert(x == 5);
};

fn control() void = {
	let x = 0;
	for (let i = 0; i < 5; i += 1) {
		if (true) {
			continue;
		};
		defer x += 1;
	};
	assert(x == 0);

	for (let i = 0; i < 5; i += 1) {
		defer x += 1;
		if (true) {
			break;
		};
		abort();
	};
	assert(x == 1);

	defer {
		yield;
	};
	defer {
		for (true) break;
		for (false) continue;
	};
};

fn reject() void = {
	let parse = [
		"export fn main() void = defer 0;",
		"export fn main() void = { if (true) defer 0; };",
		"export fn main() void = { for (defer 0; true; true) void; };",
		"export fn main() void = { defer defer 0; };",
	];
	let check = [
		"export fn main() void = { defer yield; };",
		"export fn main() void = :outer { defer { yield :outer; }; };",
		"export fn main() void = for (true) { defer break; };",
		"export fn main() void = for (true) { defer continue; };",
		"export fn main() void = { defer return; };",
		"export fn main() void = for :outer (true) { defer { break :outer; }; };",
	];

	for (let i = 0z; i < len(parse); i += 1) {
		compile(status::PARSE, parse[i])!;
	};
	for (let i = 0z; i < len(check); i += 1) {
		compile(status::CHECK, check[i])!;
	};
};

fn _never() void = {
	{
		let x = 0;
		defer x = 1;
		abort(if (x == 0) yield else "defer ran too early");
	};

	defer {
		defer abort();
		defer {
			let x = 0;
			defer x = 1;
			exit(x);
		};
	};
};

fn exit(x: int) never = {
	assert(x == 0);
	rt::exit(x);
};

fn nested() void = {
	let sl: []size = [];
	defer free(sl);
	defer {
		assert(len(sl) == 7);
		for (let i = 0z; i < len(sl); i += 1) {
			assert(sl[i] == i);
		};
	};

	defer {
		defer append(sl, 6)!;
		defer if (true) {
			append(sl, 3)!;
			defer append(sl, 5)!;
			append(sl, 4)!;
		};
		append(sl, 2)!;
	};
	defer append(sl, 1)!;
	append(sl, 0)!;
};

fn spam() void = {
	// regression test: ensure harec doesn't generate exponential IR here
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
};

fn spamfunc() (void | !void) = void;

export fn main() void = {
	basics();
	assert(x == 20);
	scope();
	loops();
	control();
	reject();
	nested();
	spam();
	_never();
};
