export type _enum = enum {
	ONE = 1,
	TWO = 2,
	THREE = 3,
};

export type other = enum {
	// purposefully something that doesn't exist in _enum
	EIGHT = 8: _enum,
};

export type enum_alias = _enum;
export type error_enum = !_enum;

export type rune_enum = enum rune {
	SEMICOLON = ';',
};

// used for a test in tests/15-enums.ha
// this is kinda a hack; it simulates a declaration in a module whose namespace
// has multiple components (in this case testmod::x)
// as of now this relies on unspecified details of harec
export type testmod::x::namespaced_alias = _enum;

export def val = 42;
export def val2: int = 90;
export def val3: enum_alias = 1: enum_alias;
export let val4 = 69;

export let @symbol("s_x") s_a: int;
export let @symbol("s_y") s_b: int = 1;

export type f64_alias = f64;
export def val5: f64_alias = 0.0;

export type uintptr_enum = enum uintptr {
	MAX = ~0u64: uintptr,
};

// ensure rt isn't imported in this subunit
static assert(SIZE_RT_SLICE == size([]opaque));

// ensure this produces valid code in a typedef
export def ptr: *int = null: *int;
