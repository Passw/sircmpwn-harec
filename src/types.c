#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "scope.h"
#include "types.h"
#include "util.h"

const struct type *
type_dereference(struct context *ctx, const struct type *type, bool allow_nullable)
{
	switch (type->storage) {
	case STORAGE_ALIAS:
		if (type_dealias(ctx, type)->storage != STORAGE_POINTER) {
			return type;
		}
		return type_dereference(ctx, type_dealias(ctx, type), allow_nullable);
	case STORAGE_POINTER:
		if (!allow_nullable && type->pointer.nullable) {
			return NULL;
		}
		return type_dereference(ctx, type->pointer.referent, allow_nullable);
	default:
		return type;
	}
}

static const struct scope_object *
complete_alias(struct context *ctx, struct type *type)
{
	assert(type->storage == STORAGE_ALIAS);
	const struct scope_object *obj =
		scope_lookup(ctx->scope, type->alias.name);
	assert(obj != NULL);
	assert(obj->otype == O_TYPE || obj->otype == O_SCAN);
	assert(obj->idecl->type == IDECL_DECL);

	if (!obj->idecl->dealias_in_progress) {
		obj->idecl->dealias_in_progress = true;
		type->alias.type = type_store_lookup_atype(
			ctx, obj->idecl->decl.type.type);
		obj->idecl->dealias_in_progress = false;
	}
	return obj;
}

const struct type *
type_dealias(struct context *ctx, const struct type *_type)
{
	struct type *type = (struct type *)_type;
	while (type->storage == STORAGE_ALIAS) {
		if (type->alias.type == NULL) {
			// gen et al. don't have access to the check context,
			// but by that point all aliases should already be fully
			// scanned
			assert(ctx != NULL);
			const struct scope_object *obj =
				complete_alias(ctx, type);
			if (type->alias.type == NULL) {
				char *identstr = ident_unparse(obj->name);
				error(ctx, obj->idecl->decl.loc, NULL,
					"Circular dependency for '%s'",
					identstr);
				free(identstr);
				type->alias.type = &builtin_type_error;
			}
		}
		type = (struct type *)type->alias.type;
	}
	return type;
}

// checks if a type is `done`, or an alias thereof, without erroring out when a
// "circular dependency" is encountered (since that means the type isn't `done`)
bool
type_is_done(struct context *ctx, const struct type *type)
{
	while (type->storage == STORAGE_ALIAS) {
		if (type->alias.type == NULL) {
			complete_alias(ctx, (struct type *)type);
			if (type->alias.type == NULL) {
				return false;
			}
		}
		type = type->alias.type;
	}
	return type->storage == STORAGE_DONE;
}

const struct struct_field *
type_get_field(struct context *ctx, const struct type *type, const char *name)
{
	if (type->storage == STORAGE_ERROR) {
		return NULL;
	}
	assert(type->storage == STORAGE_STRUCT
			|| type->storage == STORAGE_UNION);
	struct struct_field *field = type->struct_union.fields;
	while (field) {
		if (field->name) {
			if (strcmp(field->name, name) == 0) {
				return field;
			}
		} else {
			const struct struct_field *f = type_get_field(ctx,
					type_dealias(ctx, field->type), name);
			if (f != NULL) {
				return f;
			}
		}
		field = field->next;
	}
	return NULL;
}

const struct type_tuple *
type_get_value(const struct type *type, uint64_t index)
{
	assert(type->storage == STORAGE_TUPLE);
	const struct type_tuple *tuple = &type->tuple;
	while (tuple) {
		if (index == 0) {
			return tuple;
		}
		tuple = tuple->next;
		--index;
	}
	return NULL;
}

// Returns true if this type is or contains an error type
bool
type_has_error(struct context *ctx, const struct type *type)
{
	if (type->flags & TYPE_ERROR) {
		return true;
	}
	type = type_dealias(ctx, type);
	if (type->storage != STORAGE_TAGGED) {
		return false;
	}
	const struct type_tagged_union *tu = &type->tagged;
	for (; tu; tu = tu->next) {
		if (tu->type->flags & TYPE_ERROR) {
			return true;
		}
	}
	return false;
}

const char *
type_storage_unparse(enum type_storage storage)
{
	switch (storage) {
	case STORAGE_ALIAS:
		return "alias";
	case STORAGE_ARRAY:
		return "array";
	case STORAGE_BOOL:
		return "bool";
	case STORAGE_ENUM:
		return "enum";
	case STORAGE_F32:
		return "f32";
	case STORAGE_F64:
		return "f64";
	case STORAGE_ERROR:
		return "invalid";
	case STORAGE_FCONST:
		return "flexible float";
	case STORAGE_FUNCTION:
		return "function";
	case STORAGE_I16:
		return "i16";
	case STORAGE_I32:
		return "i32";
	case STORAGE_I64:
		return "i64";
	case STORAGE_I8:
		return "i8";
	case STORAGE_ICONST:
		return "flexible integer";
	case STORAGE_INT:
		return "int";
	case STORAGE_NEVER:
		return "never";
	case STORAGE_NOMEM:
		return "nomem";
	case STORAGE_NULL:
		return "null";
	case STORAGE_OPAQUE:
		return "opaque";
	case STORAGE_POINTER:
		return "pointer";
	case STORAGE_RCONST:
		return "flexible rune";
	case STORAGE_RUNE:
		return "rune";
	case STORAGE_SIZE:
		return "size";
	case STORAGE_SLICE:
		return "slice";
	case STORAGE_STRING:
		return "str";
	case STORAGE_STRUCT:
		return "struct";
	case STORAGE_TAGGED:
		return "tagged union";
	case STORAGE_TUPLE:
		return "tuple";
	case STORAGE_U16:
		return "u16";
	case STORAGE_U32:
		return "u32";
	case STORAGE_U64:
		return "u64";
	case STORAGE_U8:
		return "u8";
	case STORAGE_UINT:
		return "uint";
	case STORAGE_UINTPTR:
		return "uintptr";
	case STORAGE_UNION:
		return "union";
	case STORAGE_VALIST:
		return "valist";
	case STORAGE_VOID:
		return "void";
	case STORAGE_DONE:
		return "done";
	}
	assert(0);
}

bool
type_is_integer(struct context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_VOID:
	case STORAGE_DONE:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_OPAQUE:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_NULL:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_VALIST:
		return false;
	case STORAGE_ENUM:
	case STORAGE_ERROR:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		return true;
	case STORAGE_ALIAS:
		return type_is_integer(ctx, type_dealias(ctx, type));
	}
	assert(0); // Unreachable
}

bool
type_is_numeric(struct context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_VOID:
	case STORAGE_DONE:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_OPAQUE:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_NULL:
	case STORAGE_VALIST:
		return false;
	case STORAGE_ERROR:
	case STORAGE_ENUM:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		return true;
	case STORAGE_ALIAS:
		return type_is_numeric(ctx, type_dealias(ctx, type));
	}
	assert(0); // Unreachable
}

bool
type_is_float(struct context *ctx, const struct type *type)
{
	type = type_dealias(ctx, type);
	return type->storage == STORAGE_F32 || type->storage == STORAGE_F64
		|| type->storage == STORAGE_FCONST
		|| type->storage == STORAGE_ERROR;
}

bool
type_is_signed(struct context *ctx, const struct type *type)
{
	enum type_storage storage = type_dealias(ctx, type)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(ctx, type)->alias.type->storage;
	}
	switch (storage) {
	case STORAGE_VOID:
	case STORAGE_DONE:
	case STORAGE_ARRAY:
	case STORAGE_ENUM:
	case STORAGE_ERROR: // XXX?
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_OPAQUE:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_NULL:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VALIST:
		return false;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		return true;
	case STORAGE_ICONST:
		return type->flexible.min < 0;
	case STORAGE_ALIAS:
		assert(0); // Handled above
	}
	assert(0); // Unreachable
}

bool
type_is_flexible(const struct type *type)
{
	return type->storage == STORAGE_FCONST
		|| type->storage == STORAGE_ICONST
		|| type->storage == STORAGE_RCONST;
}

uint32_t
type_hash(const struct type *type)
{
	uint32_t hash = FNV1A_INIT;
	hash = fnv1a(hash, type->storage);
	hash = fnv1a(hash, type->flags);
	switch (type->storage) {
	case STORAGE_BOOL:
	case STORAGE_ERROR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_NULL:
	case STORAGE_OPAQUE:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VALIST:
	case STORAGE_VOID:
	case STORAGE_DONE:
	case STORAGE_STRING:
		break; // built-ins
	case STORAGE_ENUM:
		hash = fnv1a(hash, type->alias.type->storage);
		/* fallthrough */
	case STORAGE_ALIAS:
		hash = ident_hash(hash, type->alias.ident);
		break;
	case STORAGE_ARRAY:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		hash = fnv1a_size(hash, type->array.length);
		hash = fnv1a_u32(hash, type->array.expandable);
		break;
	case STORAGE_FUNCTION:
		hash = fnv1a_u32(hash, type_hash(type->func.result));
		hash = fnv1a(hash, type->func.variadism);
		for (struct type_func_param *param = type->func.params;
				param; param = param->next) {
			hash = fnv1a_u32(hash, type_hash(param->type));
			if (param->default_value) {
				hash = fnv1a_u32(hash, expr_hash(
					param->default_value));
			}
		}
		break;
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
		hash = fnv1a(hash, type->flexible.id);
		break;
	case STORAGE_POINTER:
		hash = fnv1a(hash, (unsigned char) type->pointer.nullable);
		hash = fnv1a_u32(hash, type_hash(type->pointer.referent));
		break;
	case STORAGE_SLICE:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		for (const struct struct_field *field = type->struct_union.fields;
				field; field = field->next) {
			if (field->name) {
				hash = fnv1a_s(hash, field->name);
			}
			hash = fnv1a_u32(hash, type_hash(field->type));
			hash = fnv1a_size(hash, field->offset);
		}
		break;
	case STORAGE_TAGGED:
		// Invariant: subtypes must be sorted by ID and must not include
		// any other tagged union types, nor any duplicates.
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			hash = fnv1a_u32(hash, type_hash(tu->type));
		}
		break;
	case STORAGE_TUPLE:
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			hash = fnv1a_u32(hash, type_hash(tuple->type));
		}
		break;
	}
	return hash;
}

// Note that the type this returns is NOT a type singleton and cannot be treated
// as such.
static const struct type *
strip_flags(const struct type *t, struct type *secondary)
{
	if (!t->flags) {
		return t;
	}
	*secondary = *t;
	secondary->flags = 0;
	secondary->id = type_hash(secondary);
	return secondary;
}

// Duplicate and return the tags of a tagged union
struct type_tagged_union *
tagged_dup_tags(const struct type_tagged_union *tags)
{
	struct type_tagged_union *ret = xcalloc(1, sizeof(struct type_tagged_union));
	struct type_tagged_union *cur_ret = ret;

	for (const struct type_tagged_union *tu = tags; tu; tu = tu->next) {
		if (cur_ret->type != NULL) {
			cur_ret->next = xcalloc(1, sizeof(struct type_tagged_union));
			cur_ret = cur_ret->next;
		}
		cur_ret->type = tu->type;
	}
	return ret;
}

const struct type *
tagged_select_subtype(struct context *ctx, const struct type *tagged,
		const struct type *subtype, bool strip)
{
	tagged = type_dealias(ctx, tagged);
	assert(tagged->storage == STORAGE_TAGGED);

	struct type _stripped;
	const struct type *stripped = strip_flags(subtype, &_stripped);

	size_t nassign = 0;
	const struct type *selected = NULL;
	for (const struct type_tagged_union *tu = &tagged->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == subtype->id) {
			return tu->type;
		}

		if (type_dealias(ctx, tu->type)->storage == STORAGE_VOID) {
			continue;
		}
		if (type_is_assignable(ctx, tu->type, subtype)) {
			selected = tu->type;
			++nassign;
		}
	}

	if (strip) {
		for (const struct type_tagged_union *tu = &tagged->tagged;
				tu; tu = tu->next) {
			struct type _tustripped;
			const struct type *tustripped =
				strip_flags(tu->type, &_tustripped);
			if (tustripped->id == stripped->id) {
				return tu->type;
			}
		}
	}

	if (nassign == 1) {
		return selected;
	}

	return NULL;
}

static int64_t
min_value(struct context *ctx, const struct type *t)
{
	assert(type_is_integer(ctx, t));
	if (!type_is_signed(ctx, t)) {
		return 0;
	}
	if (t->size == sizeof(int64_t)) {
		return INT64_MIN;
	}
	return -((int64_t)1 << (t->size * 8 - 1));
}

static uint64_t
max_value(struct context *ctx, const struct type *t)
{
	assert(type_is_integer(ctx, t));
	size_t bits = t->size * 8;
	if (type_is_signed(ctx, t)) {
		bits--;
	}
	if (bits == sizeof(uint64_t) * 8) {
		return UINT64_MAX;
	}
	return ((uint64_t)1 << bits) - 1;
}

const struct type *
type_create_flexible(enum type_storage storage, int64_t min, int64_t max)
{
	// XXX: This'll be impossible to free. The right solution would be to
	// store iconsts in the type store, but that'd require passing the store
	// into type_is_assignable et al. An easier solution would be to keep
	// our own list of iconsts and free them separately. Whatever, it
	// doesn't really matter that much.
	static uint32_t id = 0;
	struct type *type = xcalloc(1, sizeof(struct type));
	type->storage = storage;
	type->size = SIZE_UNDEFINED;
	type->align = ALIGN_UNDEFINED;
	type->flexible.min = min;
	type->flexible.max = max;
	type->flexible.id = id++;
	type->id = type_hash(type);
	assert(type_is_flexible(type));
	return type;
}

// Register a reference to a flexible type. When `type` is lowered in
// [[lower_flexible]], *ref will be updated to point to the new type.
void
flexible_refer(const struct type *type, const struct type **ref)
{
	if (type == NULL || !type_is_flexible(type)) {
		return;
	}
	struct type_flexible *flex = (struct type_flexible *)&type->flexible;

	if (flex->nrefs >= flex->zrefs) {
		flex->zrefs *= 2;
		if (flex->zrefs == 0) {
			flex->zrefs++;
		}
		flex->refs = xrealloc(flex->refs,
			flex->zrefs * sizeof(const struct type **));
	}
	flex->refs[flex->nrefs] = ref;
	flex->nrefs++;
}

// Sets the number of references for a flexible type to zero.
void
flexible_reset_refs(const struct type *type)
{
	if (type == NULL || !type_is_flexible(type)) {
		return;
	}
	((struct type *)type)->flexible.nrefs = 0;
}

// Lower a flexible type. If new == NULL, lower it to its default type.
const struct type *
lower_flexible(struct context *ctx, const struct type *old, const struct type *new) {
	if (!type_is_flexible(old)) {
		// If new != NULL, we're expected to always do something, and we
		// can't if it's not flexible
		assert(new == NULL);
		return old;
	}
	if (new == NULL) {
		switch (old->storage) {
		case STORAGE_FCONST:
			new = &builtin_type_f64;
			break;
		case STORAGE_ICONST:
			if (old->flexible.max <= (int64_t)max_value(ctx, &builtin_type_int)
					&& old->flexible.min >= min_value(ctx, &builtin_type_int)) {
				new = &builtin_type_int;
			} else {
				new = &builtin_type_i64;
			}
			break;
		case STORAGE_RCONST:
			new = &builtin_type_rune;
			break;
		default:
			assert(0);
		}
	}
	for (size_t i = 0; i < old->flexible.nrefs; i++) {
		flexible_refer(new, old->flexible.refs[i]);
		*old->flexible.refs[i] = new;
	}
	// XXX: Can we free old?
	return new;
}

// Implements the flexible type promotion algorithm
const struct type *
promote_flexible(struct context *ctx,
		const struct type *a, const struct type *b) {
	if (a->storage == STORAGE_ICONST && b->storage == STORAGE_ICONST) {
		int64_t min = a->flexible.min < b->flexible.min
			? a->flexible.min : b->flexible.min;
		int64_t max = a->flexible.max > b->flexible.max
			? a->flexible.max : b->flexible.max;
		const struct type *l =
			type_create_flexible(STORAGE_ICONST, min, max);
		lower_flexible(ctx, a, l);
		lower_flexible(ctx, b, l);
		return l;
	}
	if (type_is_flexible(a)) {
		if (a->storage == b->storage) {
			const struct type *l =
				type_create_flexible(a->storage, 0, 0);
			lower_flexible(ctx, a, l);
			lower_flexible(ctx, b, l);
			return l;
		}
		if (type_is_flexible(b)) {
			return NULL;
		}
		return promote_flexible(ctx, b, a);
	}
	assert(!type_is_flexible(a) && type_is_flexible(b));
	if (type_dealias(ctx, a)->storage == STORAGE_TAGGED) {
		const struct type *tag = NULL;
		for (const struct type_tagged_union *tu =
				&type_dealias(ctx, a)->tagged; tu; tu = tu->next) {
			const struct type *p = promote_flexible(ctx, tu->type, b);
			if (!p) {
				lower_flexible(ctx, b, tag);
				continue;
			}
			if (tag) {
				// Ambiguous
				b = lower_flexible(ctx, b, NULL);
				if (type_is_assignable(ctx, a, b)) {
					return b;
				}
				return NULL;
			}
			tag = p;
		}
		return tag;
	}
	switch (b->storage) {
	case STORAGE_FCONST:
		if (!type_is_float(ctx, a)) {
			return NULL;
		}
		lower_flexible(ctx, b, a);
		return a;
	case STORAGE_ICONST:
		if (!type_is_integer(ctx, a)) {
			return NULL;
		}
		if (type_is_signed(ctx, a) && min_value(ctx, a) > b->flexible.min) {
			return NULL;
		}
		if (b->flexible.max > 0 && max_value(ctx, a) < (uint64_t)b->flexible.max) {
			return NULL;
		}
		lower_flexible(ctx, b, a);
		return a;
	case STORAGE_RCONST:
		if (type_dealias(ctx, a)->storage == STORAGE_RUNE) {
			lower_flexible(ctx, b, a);
			return a;
		}
		if (!type_is_integer(ctx, a)) {
			return NULL;
		}
		if (max_value(ctx, a) < (uint64_t)b->flexible.max) {
			return NULL;
		}
		lower_flexible(ctx, b, a);
		return a;
	default:
		assert(0); // Invariant
	}
}

bool
tagged_subset_compat(struct context *ctx, const struct type *superset, const struct type *subset)
{
	// Note: this implementation depends on the invariant that tagged union
	// member types are sorted by their type ID.
	superset = type_dealias(ctx, superset), subset = type_dealias(ctx, subset);
	if (superset->storage != STORAGE_TAGGED || subset->storage != STORAGE_TAGGED) {
		return false;
	}
	const struct type_tagged_union *superset_tu = &superset->tagged,
		*subset_tu = &subset->tagged;
	while (subset_tu && superset_tu) {
		while (superset_tu) {
			if (superset_tu->type->id == subset_tu->type->id) {
				subset_tu = subset_tu->next;
				superset_tu = superset_tu->next;
				break;
			}
			superset_tu = superset_tu->next;
		}
	}

	return !subset_tu;
}

static bool
struct_subtype(struct context *ctx,
		const struct type *to, const struct type *from) {
	from = type_dealias(ctx, from);
	if (from->storage != STORAGE_STRUCT) {
		return false;
	}
	for (struct struct_field *f = from->struct_union.fields;
			f && f->offset == 0; f = f->next) {
		return f->type == to
			|| struct_subtype(ctx, to, type_dealias(ctx, f->type));
	}
	return false;
}

bool
type_is_assignable(struct context *ctx,
		const struct type *to, const struct type *from)
{
	const struct type *to_orig = to, *from_orig = from;
	if (type_dealias(ctx, to)->storage != STORAGE_TAGGED) {
		to = type_dealias(ctx, to);
		from = type_dealias(ctx, from);
	}

	// const and non-const types are mutually assignable
	struct type _to, _from;
	to = strip_flags(to, &_to), from = strip_flags(from, &_from);
	if (to->id == from->id && to->storage != STORAGE_VOID) {
		return true;
	}

	if (from->storage == STORAGE_ERROR || from->storage == STORAGE_NEVER) {
		return true;
	}

	if (type_is_flexible(from)) {
		return promote_flexible(ctx, to_orig, from_orig);
	}

	struct type _to_secondary, _from_secondary;
	const struct type *to_secondary, *from_secondary;
	switch (to->storage) {
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
		return promote_flexible(ctx, to_orig, from_orig);
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
		return type_is_integer(ctx, from)
			&& type_is_signed(ctx, from)
			&& to->size >= from->size;
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
		return type_is_integer(ctx, from)
			&& !type_is_signed(ctx, from)
			&& to->size >= from->size;
	case STORAGE_F64:
		return type_is_float(ctx, from);
	case STORAGE_POINTER:
		to_secondary = to->pointer.referent;
		to_secondary = strip_flags(to_secondary, &_to_secondary);
		switch (from->storage) {
		case STORAGE_NULL:
			return to->pointer.nullable;
		case STORAGE_POINTER:
			from_secondary = from->pointer.referent;
			from_secondary = strip_flags(from_secondary, &_from_secondary);
			if (struct_subtype(ctx, to_secondary, from_secondary)) {
				return true;
			}
			switch (to_secondary->storage) {
			case STORAGE_OPAQUE:
				break;
			case STORAGE_ARRAY:
				if (!type_is_assignable(ctx, to_secondary, from_secondary)) {
					return false;
				}
				break;
			default:
				if (to_secondary->id != from_secondary->id) {
					return false;
				}
				break;
			}
			if (from->pointer.nullable) {
				return to->pointer.nullable;
			}
			return true;
		default:
			return false;
		}
		assert(0); // Unreachable
	case STORAGE_ALIAS:
		assert(to->alias.type);
		return type_is_assignable(ctx, to->alias.type, from);
	case STORAGE_VOID:
		return to_orig->id == from_orig->id &&
			(from_orig->flags & TYPE_ERROR)	== (to_orig->flags & TYPE_ERROR);
	case STORAGE_SLICE:
		if (from->storage == STORAGE_POINTER) {
			from = type_dealias(ctx, from->pointer.referent);
			if (from->storage != STORAGE_ARRAY) {
				return false;
			}
		}
		if (from->storage != STORAGE_SLICE
				&& (from->storage != STORAGE_ARRAY
				|| from->array.length == SIZE_UNDEFINED)) {
			return false;
		}
		to_secondary = strip_flags(
			to->array.members,
			&_to_secondary);
		from_secondary = strip_flags(
			from->array.members,
			&_from_secondary);
		if (to_secondary->storage == STORAGE_OPAQUE) {
			return true;
		}
		return to_secondary->id == from_secondary->id;
	case STORAGE_ARRAY:
		if (from->storage != STORAGE_ARRAY) {
			return false;
		}
		if (from->array.expandable) {
			return to->array.length != SIZE_UNDEFINED
				&& to->array.length >= from->array.length
				&& to->array.members == from->array.members;
		} else {
			return to->array.length == SIZE_UNDEFINED
				&& to->array.members == from->array.members;
		}
	case STORAGE_TAGGED:
		return tagged_select_subtype(ctx, to, from_orig, true) != NULL
			|| tagged_subset_compat(ctx, to, from);
	// The following types are only assignable from themselves, and are
	// handled above:
	case STORAGE_BOOL:
	case STORAGE_DONE:
	case STORAGE_ENUM:
	case STORAGE_F32:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_NULL:
	case STORAGE_OPAQUE:
	case STORAGE_RUNE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UINTPTR:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		return false;
	case STORAGE_ERROR:
		return true;
	}

	assert(0); // Unreachable
}

static const struct type *
is_castable_with_tagged(struct context *ctx,
		const struct type *to, const struct type *from)
{
	if (type_dealias(ctx, from)->storage == STORAGE_TAGGED
			&& type_dealias(ctx, to)->storage == STORAGE_TAGGED) {
		if (tagged_subset_compat(ctx, to, from) || tagged_subset_compat(ctx, from, to)) {
			return to;
		}
	}
	if (type_dealias(ctx, to)->storage == STORAGE_TAGGED) {
		const struct type *subtype = tagged_select_subtype(ctx, to, from, true);
		if (subtype != NULL) {
			return subtype;
		}
	}
	if (type_dealias(ctx, from)->storage == STORAGE_TAGGED) {
		const struct type *subtype = tagged_select_subtype(ctx, from, to, true);
		if (subtype != NULL) {
			return subtype;
		}
	}
	return NULL;
}

const struct type *
type_is_castable(struct context *ctx, const struct type *to, const struct type *from)
{
	if (to->storage == STORAGE_VOID) {
		if (type_is_flexible(from)) {
			lower_flexible(ctx, from, NULL);
		}
		return to;
	} else if (to->storage == STORAGE_ERROR) {
		return to;
	}

	if (type_dealias(ctx, from)->storage == STORAGE_TAGGED
			|| type_dealias(ctx, to)->storage == STORAGE_TAGGED) {
		return is_castable_with_tagged(ctx, to, from);
	}

	const struct type *to_orig = to, *from_orig = from;
	to = type_dealias(ctx, to), from = type_dealias(ctx, from);
	if (to == from) {
		return to_orig;
	}

	struct type _to, _from;
	to = strip_flags(to, &_to), from = strip_flags(from, &_from);
	if (to->id == from->id) {
		return to_orig;
	}

	if ((!type_is_flexible(from) && from->size == SIZE_UNDEFINED)
			|| (!type_is_flexible(to) && to->size == SIZE_UNDEFINED)) {
		return NULL;
	}

	switch (from->storage) {
	case STORAGE_ICONST:
		switch (to->storage) {
		case STORAGE_F32:
		case STORAGE_F64:
			lower_flexible(ctx, from, NULL);
			return to_orig;
		case STORAGE_RUNE:
			lower_flexible(ctx, from, &builtin_type_u32);
			return to_orig;
		default:
			return promote_flexible(ctx, from_orig, to_orig);
		}
		break;
	case STORAGE_FCONST:
		if (type_is_integer(ctx, to)) {
			lower_flexible(ctx, from, NULL);
			return to_orig;
		}
		// fallthrough
	case STORAGE_RCONST:
		return promote_flexible(ctx, from_orig, to_orig);
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
		return to->storage == STORAGE_ENUM
			|| type_is_numeric(ctx, to)
			|| to->storage == STORAGE_RUNE
			? to_orig : NULL;
	case STORAGE_RUNE:
		return type_is_integer(ctx, to)
			? to_orig : NULL;
	case STORAGE_ENUM:
		if (from->alias.type->storage == STORAGE_RUNE) {
			return to->storage == STORAGE_RUNE ? to_orig : NULL;
		}
		return to->storage == STORAGE_ENUM || type_is_integer(ctx, to)
			? to_orig : NULL;
	case STORAGE_F32:
	case STORAGE_F64:
		return type_is_numeric(ctx, to)
			? to_orig : NULL;
	case STORAGE_UINTPTR:
		return to->storage == STORAGE_POINTER
			|| to->storage == STORAGE_NULL
			|| type_is_numeric(ctx, to)
			|| to->storage == STORAGE_ENUM
			? to_orig : NULL;
	case STORAGE_POINTER:
		return to->storage == STORAGE_POINTER
			|| to->storage == STORAGE_NULL
			|| to->storage == STORAGE_UINTPTR
			? to_orig : NULL;
	case STORAGE_NULL:
		return to->storage == STORAGE_POINTER
			|| to->storage == STORAGE_UINTPTR
			? to_orig : NULL;
	case STORAGE_SLICE:
		return to->storage == STORAGE_SLICE
			|| (to->storage == STORAGE_POINTER
					&& to->pointer.referent->storage == STORAGE_ARRAY)
			? to_orig : NULL;
	case STORAGE_ARRAY:
		return to->storage == STORAGE_ARRAY
			|| to->storage == STORAGE_SLICE
			? to_orig : NULL;
	// Cannot be cast:
	case STORAGE_STRING:
	case STORAGE_BOOL:
	case STORAGE_VOID:
	case STORAGE_DONE:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_OPAQUE:
	case STORAGE_FUNCTION:
	case STORAGE_TUPLE:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		return NULL;
	case STORAGE_ERROR:
	case STORAGE_TAGGED:
	case STORAGE_ALIAS:
		assert(0); // Handled above
	}

	assert(0); // Unreachable
}

void
builtin_types_init(const char *target)
{
	if (strcmp(target, "aarch64") == 0) {
		builtin_type_f64.align = 8;
		builtin_type_int.size = 4;
		builtin_type_int.align = 4;
		builtin_type_uint.size = 4;
		builtin_type_uint.align = 4;
		builtin_type_uintptr.size = 8;
		builtin_type_uintptr.align = 8;
		builtin_type_i64.align = 8;
		builtin_type_u64.align = 8;
		builtin_type_null.size = 8;
		builtin_type_null.align = 8;
		builtin_type_size.size = 8;
		builtin_type_size.align = 8;
		builtin_type_str.size = 24;
		builtin_type_str.align = 8;
		builtin_type_valist.size = 32;
		builtin_type_valist.align = 8;
	} else if (strcmp(target, "riscv64") == 0) {
		builtin_type_f64.align = 8;
		builtin_type_int.size = 4;
		builtin_type_int.align = 4;
		builtin_type_uint.size = 4;
		builtin_type_uint.align = 4;
		builtin_type_uintptr.size = 8;
		builtin_type_uintptr.align = 8;
		builtin_type_i64.align = 8;
		builtin_type_u64.align = 8;
		builtin_type_null.size = 8;
		builtin_type_null.align = 8;
		builtin_type_size.size = 8;
		builtin_type_size.align = 8;
		builtin_type_str.size = 24;
		builtin_type_str.align = 8;
		builtin_type_valist.size = 8;
		builtin_type_valist.align = 8;
	} else if (strcmp(target, "x86_64") == 0) {
		builtin_type_f64.align = 8;
		builtin_type_int.size = 4;
		builtin_type_int.align = 4;
		builtin_type_uint.size = 4;
		builtin_type_uint.align = 4;
		builtin_type_uintptr.size = 8;
		builtin_type_uintptr.align = 8;
		builtin_type_i64.align = 8;
		builtin_type_u64.align = 8;
		builtin_type_null.size = 8;
		builtin_type_null.align = 8;
		builtin_type_size.size = 8;
		builtin_type_size.align = 8;
		builtin_type_str.size = 24;
		builtin_type_str.align = 8;
		builtin_type_valist.size = 24;
		builtin_type_valist.align = 8;
	} else {
		xfprintf(stderr, "Unsupported or unrecognized target: %s\n", target);
		exit(EXIT_USER);
	}
	struct type *builtins[] = {
		&builtin_type_bool, &builtin_type_error, &builtin_type_f32,
		&builtin_type_f64, &builtin_type_i8, &builtin_type_i16,
		&builtin_type_i32, &builtin_type_i64, &builtin_type_int,
		&builtin_type_u8, &builtin_type_u16, &builtin_type_u32,
		&builtin_type_u64, &builtin_type_uint, &builtin_type_uintptr,
		&builtin_type_null, &builtin_type_rune, &builtin_type_size,
		&builtin_type_void, &builtin_type_done, &builtin_type_nomem,
		&builtin_type_str, &builtin_type_valist,
	};
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i) {
		builtins[i]->id = type_hash(builtins[i]);
	}
}

// Built-in type singletons
struct type builtin_type_bool = {
	.storage = STORAGE_BOOL,
	.size = 1,
	.align = 1,
},
builtin_type_error = {
	.storage = STORAGE_ERROR,
	.size = 0,
	.align = 0,
},
builtin_type_f32 = {
	.storage = STORAGE_F32,
	.size = 4,
	.align = 4,
},
builtin_type_f64 = {
	.storage = STORAGE_F64,
	.size = 8,
},
builtin_type_i8 = {
	.storage = STORAGE_I8,
	.size = 1,
	.align = 1,
},
builtin_type_i16 = {
	.storage = STORAGE_I16,
	.size = 2,
	.align = 2,
},
builtin_type_i32 = {
	.storage = STORAGE_I32,
	.size = 4,
	.align = 4,
},
builtin_type_i64 = {
	.storage = STORAGE_I64,
	.size = 8,
},
builtin_type_int = {
	.storage = STORAGE_INT,
},
builtin_type_never = {
	.storage = STORAGE_NEVER,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_nomem = {
	.storage = STORAGE_NOMEM,
	.flags = TYPE_ERROR,
	.size = 0,
	.align = 0,
},
builtin_type_opaque = {
	.storage = STORAGE_OPAQUE,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_u8 = {
	.storage = STORAGE_U8,
	.size = 1,
	.align = 1,
},
builtin_type_u16 = {
	.storage = STORAGE_U16,
	.size = 2,
	.align = 2,
},
builtin_type_u32 = {
	.storage = STORAGE_U32,
	.size = 4,
	.align = 4,
},
builtin_type_u64 = {
	.storage = STORAGE_U64,
	.size = 8,
},
builtin_type_uint = {
	.storage = STORAGE_UINT,
},
builtin_type_uintptr = {
	.storage = STORAGE_UINTPTR,
},
builtin_type_null = {
	.storage = STORAGE_NULL,
},
builtin_type_rune = {
	.storage = STORAGE_RUNE,
	.size = 4,
	.align = 4,
},
builtin_type_size = {
	.storage = STORAGE_SIZE,
},
builtin_type_void = {
	.storage = STORAGE_VOID,
	.size = 0,
	.align = 0,
},
builtin_type_done = {
	.storage = STORAGE_DONE,
	.size = 0,
	.align = 0,
},
builtin_type_str = {
	.storage = STORAGE_STRING,
},
builtin_type_valist = {
	.storage = STORAGE_VALIST,
};
