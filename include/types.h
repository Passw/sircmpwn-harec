#ifndef HARE_TYPES_H
#define HARE_TYPES_H
#include <stdbool.h>
#include <stdint.h>
#include "identifier.h"

enum type_storage {
	// Built-in types
	// The order of these is important
	STORAGE_BOOL,
	STORAGE_DONE,
	STORAGE_F32,
	STORAGE_F64,
	STORAGE_I16,
	STORAGE_I32,
	STORAGE_I64,
	STORAGE_I8,
	STORAGE_INT,
	STORAGE_NEVER,
	STORAGE_NOMEM,
	STORAGE_NULL,
	STORAGE_OPAQUE,
	STORAGE_RUNE,
	STORAGE_SIZE,
	STORAGE_STRING,
	STORAGE_U16,
	STORAGE_U32,
	STORAGE_U64,
	STORAGE_U8,
	STORAGE_UINT,
	STORAGE_UINTPTR,
	STORAGE_VOID,
	// Other types
	STORAGE_ALIAS,
	STORAGE_ARRAY,
	STORAGE_ENUM,
	STORAGE_FUNCTION,
	STORAGE_POINTER,
	STORAGE_SLICE,
	STORAGE_STRUCT,
	STORAGE_TAGGED,
	STORAGE_TUPLE,
	STORAGE_UNION,
	STORAGE_VALIST,
	STORAGE_FCONST,
	STORAGE_ICONST,
	STORAGE_RCONST,
	// For internal use only
	STORAGE_ERROR,
};

struct context;
struct type;

#define SIZE_UNDEFINED ((size_t)-1)
#define ALIGN_UNDEFINED ((size_t)-1)

struct type_alias {
	struct ident *ident;
	struct ident *name;
	const struct type *type;
	bool exported; // Used to make sure unexported aliases aren't emitted
};

struct type_array {
	size_t length; // SIZE_UNDEFINED for [*] and slices
	const struct type *members;
	bool expandable;
};

struct type_enum {
	struct scope *values;
};

enum variadism {
	VARIADISM_NONE,
	VARIADISM_C,
	VARIADISM_HARE,
};

struct type_func_param {
	const struct type *type;
	struct expression *default_value;
	struct type_func_param *next;
};

struct type_func {
	const struct type *result;
	enum variadism variadism;
	struct type_func_param *params;
};

struct type_flexible {
	int64_t min, max;
	uint32_t id;
	const struct type ***refs;
	size_t nrefs;
	size_t zrefs;
};

struct type_pointer {
	const struct type *referent;
	bool nullable;
};

struct struct_field {
	const char *name;
	const struct type *type;
	size_t offset;
	size_t size;
	struct struct_field *next;
};

struct type_struct_union {
	struct struct_field *fields;
	bool packed;
};

struct type_tuple {
	const struct type *type;
	size_t offset;
	struct type_tuple *next;
};

struct type_tagged_union {
	const struct type *type;
	struct type_tagged_union *next;
};

enum type_flags {
	TYPE_ERROR = 1 << 0,
};

struct type {
	enum type_storage storage;
	uint32_t id;
	unsigned int flags;
	size_t size, align;
	union {
		struct {
			struct type_alias alias;
			struct type_enum _enum;
		};
		struct type_array array;
		struct type_flexible flexible;
		struct type_func func;
		struct type_pointer pointer;
		struct type_struct_union struct_union;
		struct type_tagged_union tagged;
		struct type_tuple tuple;
	};
};

const struct type *type_dereference(struct context *ctx, const struct type *type,
		bool allow_nullable);
const struct type *type_dealias(struct context *ctx, const struct type *type);
bool type_is_done(struct context *ctx, const struct type *type);
const struct struct_field *type_get_field(struct context *ctx,
	const struct type *type, const char *name);
const struct type_tuple *type_get_value(
	const struct type *type, uint64_t index);

struct type_tagged_union * 
tagged_dup_tags(const struct type_tagged_union *tags);
const struct type *tagged_select_subtype(struct context *ctx,
	const struct type *tagged, const struct type *subtype, bool strip);
bool tagged_subset_compat(struct context *ctx,
	const struct type *to, const struct type *from);

const char *type_storage_unparse(enum type_storage storage);
bool type_is_signed(struct context *ctx, const struct type *type);
bool type_is_integer(struct context *ctx, const struct type *type);
bool type_is_numeric(struct context *ctx, const struct type *type);
bool type_is_float(struct context *ctx, const struct type *type);
bool type_is_flexible(const struct type *type);
bool type_has_error(struct context *ctx, const struct type *type);

uint32_t type_hash(const struct type *type);

const struct type *promote_flexible(struct context *ctx,
	const struct type *a, const struct type *b);
bool type_is_assignable(struct context *ctx,
	const struct type *to, const struct type *from);
const struct type *type_is_castable(struct context *ctx,
	const struct type *to, const struct type *from);

const struct type *type_create_flexible(enum type_storage storage,
	int64_t min, int64_t max);
const struct type *lower_flexible(struct context *ctx,
	const struct type *old, const struct type *new);
void flexible_refer(const struct type *type, const struct type **ref);
void flexible_reset_refs(const struct type *type);

void builtin_types_init(const char *target);

// Built-in type singletons
extern struct type
	// Primitive
	builtin_type_bool,
	builtin_type_done,
	builtin_type_error,
	builtin_type_f32,
	builtin_type_f64,
	builtin_type_i16,
	builtin_type_i32,
	builtin_type_i64,
	builtin_type_i8,
	builtin_type_int,
	builtin_type_never,
	builtin_type_nomem,
	builtin_type_null,
	builtin_type_opaque,
	builtin_type_rune,
	builtin_type_size,
	builtin_type_u16,
	builtin_type_u32,
	builtin_type_u64,
	builtin_type_u8,
	builtin_type_uint,
	builtin_type_uintptr,
	builtin_type_void,

	// etc
	builtin_type_str,
	builtin_type_valist;

#endif
