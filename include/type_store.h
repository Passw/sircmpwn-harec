#ifndef HARE_TYPESTORE_H
#define HARE_TYPESTORE_H
#include "ast.h"
#include "lex.h"
#include "types.h"

#define TYPE_STORE_BUCKETS 65536

struct type_bucket {
	struct type type;
	struct type_bucket *next;
};

struct context;

struct dimensions {
	size_t size;
	size_t align;
};

typedef struct type_bucket *type_store[TYPE_STORE_BUCKETS];

// Applies the type reduction algorithm to the given tagged union.
const struct type *type_store_reduce_result(struct context *ctx,
		struct location loc, struct type_tagged_union *in);

struct ast_type;

const struct type *type_store_lookup_atype(
	struct context *ctx, const struct ast_type *atype);

struct dimensions type_store_lookup_dimensions(
	struct context *ctx, const struct ast_type *atype);

const struct type *builtin_type_for_storage(enum type_storage storage);

const struct type *type_store_lookup_with_flags(struct context *ctx,
	const struct type *type, unsigned int flags);

const struct type *type_store_lookup_pointer(struct context *ctx,
	struct location loc, const struct type *referent, bool nullable);

const struct type *type_store_lookup_array(struct context *ctx,
	struct location loc, const struct type *members, size_t len,
	bool expandable);

const struct type *type_store_lookup_slice(struct context *ctx,
	struct location loc, const struct type *members);

const struct type *type_store_lookup_alias(struct context *ctx,
	struct ident *ident, struct ident *name,
	const struct type *secondary, int flags, bool exported);

const struct type *type_store_lookup_tagged(struct context *ctx,
	struct location loc, struct type_tagged_union *tags);

const struct type *type_store_lookup_tuple(struct context *ctx,
	struct location loc, struct type_tuple *values);

const struct type *type_store_lookup_enum(struct context *ctx,
	const struct ast_type *atype, bool exported);

#endif
