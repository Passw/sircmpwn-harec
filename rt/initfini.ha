@symbol("__hare_init_array_start") const init_start: [*]*fn() void;
@symbol("__hare_init_array_end") const init_end: [*]*fn() void;
@symbol("__hare_fini_array_start") const fini_start: [*]*fn() void;
@symbol("__hare_fini_array_end") const fini_end: [*]*fn() void;

export fn init() void = {
	const ninit = (&init_end: uintptr - &init_start: uintptr): size
		/ size(*fn() void);
	for (let i = 0z; i < ninit; i += 1) {
		init_start[i]();
	};
};

export fn fini() void = {
	const nfini = (&fini_end: uintptr - &fini_start: uintptr): size
		/ size(*fn() void);
	for (let i = 0z; i < nfini; i += 1) {
		fini_start[i]();
	};
};
