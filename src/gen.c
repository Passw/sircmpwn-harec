#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static const struct gen_value gv_void = {
	.kind = GV_CONST,
	.type = &builtin_type_void,
};

static struct gen_value gen_expr(struct gen_context *ctx,
	const struct expression *expr);
static void gen_expr_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out);
static void gen_expr_branch(struct gen_context *ctx,
	const struct expression *expr, struct gen_value merged,
	struct gen_value *out);
static void gen_global_decl(struct gen_context *ctx,
	const struct declaration *decl);

static struct gen_scope *
gen_scope_lookup(struct gen_context *ctx, const struct scope *which)
{
	for (struct gen_scope *scope = ctx->scope;
			scope; scope = scope->parent) {
		if (scope->scope == which) {
			return scope;
		}
	}
	abort();
}

static struct gen_scope *
push_scope(struct gen_context *ctx, const struct scope *scope)
{
	struct gen_scope *new = xcalloc(1, sizeof(struct gen_scope));
	new->parent = ctx->scope;
	new->scope = scope;
	ctx->scope = new;
	return new;
}

static void
pop_scope(struct gen_context *ctx)
{
	struct gen_scope *scope = ctx->scope;
	ctx->scope = scope->parent;
	for (struct gen_defer *defer = scope->defers; defer; /* n/a */) {
		struct gen_defer *next = defer->next;
		free(defer);
		defer = next;
	}
	free(scope);
}

static void
gen_defers(struct gen_context *ctx, struct gen_scope *scope)
{
	if (!scope) {
		return;
	}
	if (scope->defers) {
		pushc(ctx->current, "gen defers");
	}
	struct gen_defer *defers = scope->defers;
	while (scope->defers) {
		struct gen_defer *defer = scope->defers;
		assert(defer->expr->type == EXPR_DEFER);
		scope->defers = scope->defers->next;
		push_scope(ctx, defer->expr->defer.scope);
		gen_expr(ctx, defer->expr->defer.deferred);
		pop_scope(ctx);
	}
	scope->defers = defers;
}

static void
gen_copy_memcpy(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	struct qbe_value dtemp = mklval(ctx, &dest);
	struct qbe_value stemp = mklval(ctx, &src);
	struct qbe_value sz = constl(dest.type->size);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &dtemp, &stemp, &sz, NULL);
}

static void
gen_copy_aligned(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	if (dest.type->size > 128) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	struct qbe_value srcv = mkqval(ctx, &src);
	struct qbe_value destv = mkqval(ctx, &dest);
	struct qbe_value size = constl(dest.type->size);
	pushi(ctx->current, NULL, Q_BLIT, &srcv, &destv, &size, NULL);
}

static void
gen_store(struct gen_context *ctx,
	struct gen_value object,
	struct gen_value value)
{
	const struct type *ty = type_dealias(NULL, object.type);
	if (value.type->size == 0 || value.type->storage == STORAGE_NEVER) {
		return; // no storage
	}
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_VALIST:
		gen_copy_aligned(ctx, object, value);
		return;
	case STORAGE_ENUM:
		object.type = ty->alias.type;
		break;
	default:
		break; // no-op
	}

	struct qbe_value qobj = mkqval(ctx, &object),
		qval = mkqval(ctx, &value);
	enum qbe_instr qi = store_for_type(ctx, object.type);
	pushi(ctx->current, NULL, qi, &qval, &qobj, NULL);
}

// Generates code to store the type ID (id) in a tagged union's tag field (at
// address "out" and of type "tagged")
static void
gen_store_tag(struct gen_context *ctx,
	struct qbe_value *out,
	const struct type *tagged,
	struct qbe_value *id)
{
	assert(type_dealias(NULL, tagged)->storage == STORAGE_TAGGED);
	const enum qbe_instr store = store_for_type(ctx, &builtin_type_u32);
	pushi(ctx->current, NULL, store, id, out, NULL);
}

static struct gen_value
gen_load(struct gen_context *ctx, struct gen_value object)
{
	const struct type *ty = type_dealias(NULL, object.type);
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		return object;
	case STORAGE_ENUM:
		object.type = ty->alias.type;
		break;
	default:
		break; // no-op
	}

	struct gen_value value = mkgtemp(ctx, object.type, ".%d");
	struct qbe_value qobj = mkqval(ctx, &object),
		qval = mkqval(ctx, &value);
	enum qbe_instr qi = load_for_type(ctx, object.type);
	pushi(ctx->current, &qval, qi, &qobj, NULL);
	return value;
}

// Generates code to load the type ID (into "out") from the tag field of a
// tagged union (at "from" and of type "tagged").
static void
gen_load_tag(struct gen_context *ctx,
	struct qbe_value *out,
	struct qbe_value *from,
	const struct type *tagged)
{
	assert(type_dealias(NULL, tagged)->storage == STORAGE_TAGGED);
	const enum qbe_instr load = load_for_type(ctx, &builtin_type_u32);
	pushi(ctx->current, out, load, from, NULL);
}

static void
gen_fixed_abort(struct gen_context *ctx,
	struct location loc, enum fixed_aborts reason)
{
	struct qbe_value path = mklval(ctx, &ctx->sources[loc.file]);
	struct qbe_value line = constl(loc.lineno);
	struct qbe_value col = constl(loc.colno);
	struct qbe_value tmp = constl(reason);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.fixedabort,
			&path, &line, &col, &tmp, NULL);
	pushi(ctx->current, NULL, Q_HLT, NULL);
}

static void
gen_fixed_assert(struct gen_context *ctx,
	struct location loc,
	enum fixed_aborts reason,
	enum qbe_instr cmp,
	struct qbe_value *lvalue,
	struct qbe_value *rvalue)
{
	struct qbe_value cond = mkqtmp(ctx, &qbe_word, "cond.%d");
	pushi(ctx->current, &cond, cmp, lvalue, rvalue, NULL);

	struct qbe_statement lfail, lpass;
	struct qbe_value bfail = mklabel(ctx, &lfail, "abort.%d");
	struct qbe_value bpass = mklabel(ctx, &lpass, "pass.%d");

	pushi(ctx->current, NULL, Q_JNZ, &cond, &bpass, &bfail, NULL);
	push(&ctx->current->body, &lfail);
	gen_fixed_abort(ctx, loc, reason);
	push(&ctx->current->body, &lpass);
}

static struct gen_value
gen_autoderef_expr(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value val = gen_expr(ctx, expr);
	while (type_dealias(NULL, val.type)->storage == STORAGE_POINTER) {
		val.type = type_dealias(NULL, val.type)->pointer.referent;
		val = gen_load(ctx, val);
	}
	return val;
}

struct gen_slice
gen_slice_ptrs(struct gen_context *ctx, struct gen_value object)
{
	struct gen_slice slice = {
		.base = mkqval(ctx, &object),
		.len = mkqtmp(ctx, ctx->arch.ptr, ".%d"),
		.cap = mkqtmp(ctx, ctx->arch.ptr, ".%d"),
	};
	struct qbe_value lenoff = constl(ctx->arch.ptr->size);
	struct qbe_value capoff = constl(ctx->arch.ptr->size + ctx->arch.sz->size);
	pushi(ctx->current, &slice.len, Q_ADD, &slice.base, &lenoff, NULL);
	pushi(ctx->current, &slice.cap, Q_ADD, &slice.base, &capoff, NULL);
	return slice;
}

void
load_slice_data(struct gen_context *ctx, struct gen_slice *slobj,
		struct qbe_value *base, struct qbe_value *len,
		struct qbe_value *cap)
{
	enum qbe_instr ptrload = load_for_type(ctx, &builtin_type_uintptr);
	enum qbe_instr szload = load_for_type(ctx, &builtin_type_size);
	if (base) {
		*base = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, base, ptrload, &slobj->base, NULL);
	}
	if (len) {
		*len = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, len, szload, &slobj->len, NULL);
	}
	if (cap) {
		*cap = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, cap, szload, &slobj->cap, NULL);
	}
}

void
store_slice_data(struct gen_context *ctx, struct gen_slice *slobj,
		struct qbe_value *base, struct qbe_value *len,
		struct qbe_value *cap)
{
	enum qbe_instr szstore = store_for_type(ctx, &builtin_type_size);
	enum qbe_instr ptrstore = store_for_type(ctx, &builtin_type_uintptr);
	if (base) {
		pushi(ctx->current, NULL, ptrstore, base, &slobj->base, NULL);
	}
	if (len) {
		pushi(ctx->current, NULL, szstore, len, &slobj->len, NULL);
	}
	if (cap) {
		pushi(ctx->current, NULL, szstore, cap, &slobj->cap, NULL);
	}
}

static struct gen_value
gen_access_ident(struct gen_context *ctx, const struct scope_object *obj)
{
	switch (obj->otype) {
	case O_BIND:
		for (const struct gen_binding *gb = ctx->bindings;
				gb; gb = gb->next) {
			if (gb->object == obj) {
				return gb->value;
			}
		}
		abort();
	case O_DECL:
		return (struct gen_value){
			.kind = GV_GLOBAL,
			.type = obj->type,
			.name = ident_to_sym(ctx->itbl, obj->ident),
			.threadlocal = obj->flags & SO_THREADLOCAL,
		};
	case O_CONST:
	case O_TYPE:
	case O_SCAN:
		break;
	}
	abort(); // Invariant
}

static struct gen_value
gen_access_index(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value glval = gen_autoderef_expr(ctx, expr->access.array);
	struct gen_value out = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qival = mklval(ctx, &out);
	bool checkbounds = !expr->access.bounds_checked;
	struct qbe_value length, qlval;
	const struct type *ty = type_dealias(NULL, glval.type);
	switch (ty->storage) {
	case STORAGE_SLICE:;
		struct gen_slice sl = gen_slice_ptrs(ctx, glval);
		load_slice_data(ctx, &sl, &qlval, &length, NULL);
		break;
	case STORAGE_ARRAY:
		qlval = mkqval(ctx, &glval);
		if (ty->array.length != SIZE_UNDEFINED) {
			length = constl(ty->array.length);
		} else {
			checkbounds = false;
		}
		break;
	default:
		assert(0); // Unreachable
	}

	struct gen_value index = gen_expr(ctx, expr->access.index);
	struct qbe_value qindex = mkqval(ctx, &index);
	struct qbe_value itemsz = constl(expr->result->size);
	pushi(ctx->current, &qival, Q_MUL, &qindex, &itemsz, NULL);
	pushi(ctx->current, &qival, Q_ADD, &qlval, &qival, NULL);

	if (checkbounds) {
		gen_fixed_assert(ctx, expr->loc, ABORT_OOB, Q_CULTL, &qindex, &length);
	}

	return out;
}

static struct gen_value
gen_access_field(struct gen_context *ctx, const struct expression *expr)
{
	const struct struct_field *field = expr->access.field;
	struct gen_value glval = gen_autoderef_expr(ctx, expr->access._struct);
	if (field->type->size == 0) {
		return gv_void;
	}
	struct gen_value out = mkgtemp(ctx, field->type, "field.%d");
	struct qbe_value qfval = mklval(ctx, &out);
	struct qbe_value qlval = mkqval(ctx, &glval);
	struct qbe_value offs = constl(field->offset);
	pushi(ctx->current, &qfval, Q_ADD, &qlval, &offs, NULL);
	return out;
}

static struct gen_value
gen_access_value(struct gen_context *ctx, const struct expression *expr)
{
	const struct type_tuple *tuple = expr->access.tvalue;
	struct gen_value glval = gen_autoderef_expr(ctx, expr->access.tuple);
	if (tuple->type->size == 0) {
		return gv_void;
	}
	struct gen_value out = mkgtemp(ctx, tuple->type, "field.%d");
	struct qbe_value qfval = mklval(ctx, &out);
	struct qbe_value qlval = mkqval(ctx, &glval);
	struct qbe_value offs = constl(tuple->offset);
	pushi(ctx->current, &qfval, Q_ADD, &qlval, &offs, NULL);
	return out;
}

static struct gen_value
gen_expr_access_addr(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value addr;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		if (expr->access.object->type->size == 0) {
			return gv_void;
		}
		addr = gen_access_ident(ctx, expr->access.object);
		break;
	case ACCESS_INDEX:
		addr = gen_access_index(ctx, expr);
		break;
	case ACCESS_FIELD:
		if (expr->access._struct->result->size == 0) {
			return gv_void;
		}
		addr = gen_access_field(ctx, expr);
		break;
	case ACCESS_TUPLE:
		if (expr->access.tuple->result->size == 0) {
			return gv_void;
		}
		addr = gen_access_value(ctx, expr);
		break;
	}
	return addr;
}

static struct gen_value
gen_expr_access(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value addr = gen_expr_access_addr(ctx, expr);
	if (expr->result->size == 0) {
		return gv_void;
	}
	return gen_load(ctx, addr);
}

static struct gen_value
gen_expr_alloc_slice_array_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	struct qbe_value qout = mklval(ctx, &out);

	enum alloc_kind kind = expr->alloc.kind;

	struct qbe_value qcap;
	if (kind == ALLOC_LEN || kind == ALLOC_CAP) {
		struct gen_value cap = gen_expr(ctx, expr->alloc.cap);
		qcap = mkqval(ctx, &cap);
	}

	struct qbe_value length, initdata;
	const struct type *inittype = type_dealias(NULL, expr->alloc.init->result);
	if (inittype->storage == STORAGE_SLICE) {
		assert(kind != ALLOC_LEN);
		struct gen_value init = gen_expr(ctx, expr->alloc.init);
		struct gen_slice sl = gen_slice_ptrs(ctx, init);
		load_slice_data(ctx, &sl, &initdata, &length, NULL);
	} else if (inittype->storage == STORAGE_ARRAY) {
		assert(inittype->array.length != SIZE_UNDEFINED);
		length = constl(inittype->array.length);
	} else {
		abort(); // invariant
	}

	if (kind == ALLOC_LEN || kind == ALLOC_CAP) {
		gen_fixed_assert(ctx, expr->loc, ABORT_CAP_TOO_SMALL,
				Q_CULEL, &length, &qcap);
	} else {
		qcap = length;
	}

	struct qbe_value qnull = constl(0);
	struct qbe_value qisnull = mkqtmp(ctx, &qbe_word, ".%d");
	struct gen_value newdata = mkgtemp(ctx, inittype, ".%d");
	struct qbe_value qnewdata = mklval(ctx, &newdata);

	struct qbe_statement lzero, lnonzero, loom, lvalid, lend;
	struct qbe_value bzero = mklabel(ctx, &lzero, "zero.%d");
	struct qbe_value bnonzero = mklabel(ctx, &lnonzero, "nonzero.%d");
	struct qbe_value boom = mklabel(ctx, &loom, "oom.%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, "valid.%d");
	struct qbe_value bend = mklabel(ctx, &lend, "end.%d");

	const struct type *sltype = type_dealias(NULL, expr->alloc.allocation_result);
	assert(sltype->storage == STORAGE_SLICE);
	struct qbe_value sltype_id = constw(sltype->id);

	pushi(ctx->current, &qnewdata, Q_COPY, &qnull, NULL);

	// don't allocate if cap == 0
	pushi(ctx->current, &qisnull, Q_CEQL, &qcap, &qnull, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &qisnull, &bzero, &bnonzero, NULL);

	push(&ctx->current->body, &lnonzero);

	struct qbe_value sz = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value membsz = constl(sltype->array.members->size);
	pushi(ctx->current, &sz, Q_MUL, &membsz, &qcap, NULL);

	pushi(ctx->current, &qnewdata, Q_CALL, &ctx->rt.malloc, &sz, NULL);

	pushi(ctx->current, &qisnull, Q_CEQL, &qnewdata, &qnull, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &qisnull, &boom, &bvalid, NULL);

	push(&ctx->current->body, &lvalid);

	if (inittype->storage == STORAGE_ARRAY) {
		gen_expr_at(ctx, expr->alloc.init, newdata);

		if (kind == ALLOC_LEN) {
			struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, ".%d");
			struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
			pushi(ctx->current, &next, Q_MUL, &length, &membsz, NULL);
			pushi(ctx->current, &next, Q_ADD, &next, &qnewdata, NULL);
			pushi(ctx->current, &last, Q_SUB, &next, &membsz, NULL);

			struct qbe_value remain = mkqtmp(ctx, ctx->arch.sz, ".%d");
			pushi(ctx->current, &remain, Q_SUB, &qcap, &length, NULL);
			pushi(ctx->current, &remain, Q_MUL, &remain, &membsz, NULL);
			pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &remain, NULL);

			length = qcap;
		}
	} else {
		struct qbe_value bytelen = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &bytelen, Q_MUL, &length, &membsz, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy,
			&qnewdata, &initdata, &bytelen, NULL);
	}

	// cap == 0 case
	push(&ctx->current->body, &lzero);

	struct gen_value gslice_out = mkgtemp(ctx, sltype, ".%d");
	struct qbe_value qslice_out = mklval(ctx, &gslice_out);
	struct qbe_value qslice_offset = compute_tagged_memb_offset(sltype);

	pushi(ctx->current, &qslice_out, Q_ADD, &qout, &qslice_offset, NULL);

	struct gen_slice sl = gen_slice_ptrs(ctx, gslice_out);
	store_slice_data(ctx, &sl, &qnewdata, &length, &qcap);
	gen_store_tag(ctx, &qout, expr->result, &sltype_id);

	pushi(ctx->current, NULL, Q_JMP, &bend, NULL);

	// out of memory (OOM) case
	push(&ctx->current->body, &loom);
	struct qbe_value nomem_id = constw(builtin_type_nomem.id);
	gen_store_tag(ctx, &qout, expr->result, &nomem_id);

	push(&ctx->current->body, &lend);
	return out;
}

static struct gen_value
gen_expr_alloc_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	struct gen_value ret;
	struct qbe_value qout;

	if (!out) {
		ret = mkgtemp(ctx, expr->result, "object.%d");
		out = &ret;
		qout = mkqval(ctx, out);

		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &qout, alloc, &sz, NULL);
	} else {
		qout = mklval(ctx, out);
	}

	if (expr->alloc.kind != ALLOC_OBJECT) {
		return gen_expr_alloc_slice_array_at(ctx, expr, *out);
	}

	// alloc(init) case

	struct qbe_statement loom, lvalid, lend;
	struct qbe_value boom = mklabel(ctx, &loom, "oom.%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, "valid.%d");
	struct qbe_value bend = mklabel(ctx, &lend, "end.%d");

	assert(expr->alloc.cap == NULL);

	const struct type *objtype =
		expr->alloc.allocation_result->pointer.referent;

	struct qbe_value sz = constl(objtype->size);
	struct gen_value result = mkgtemp(ctx, objtype, ".%d");
	struct qbe_value qresult = mklval(ctx, &result);
	pushi(ctx->current, &qresult, Q_CALL, &ctx->rt.malloc, &sz, NULL);

	struct qbe_value qnull = constl(0);
	struct qbe_value qisnull = mkqtmp(ctx, &qbe_word, ".%d");
	pushi(ctx->current, &qisnull, Q_CEQL, &qresult, &qnull, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &qisnull, &boom, &bvalid, NULL);

	push(&ctx->current->body, &lvalid);

	struct qbe_value qoffs = compute_tagged_memb_offset(expr->alloc.allocation_result);
	struct qbe_value qptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value qtag = constw(expr->alloc.allocation_result->id);

	pushi(ctx->current, &qptr, Q_ADD, &qout, &qoffs, NULL);

	// XXX: should use gen_store()
	enum qbe_instr store = store_for_type(ctx, &builtin_type_uintptr);
	pushi(ctx->current, NULL, store, &qresult, &qptr, NULL);
	gen_store_tag(ctx, &qout, expr->result, &qtag);

	gen_expr_at(ctx, expr->alloc.init, result);

	pushi(ctx->current, NULL, Q_JMP, &bend, NULL);

	push(&ctx->current->body, &loom);

	qtag = constw(builtin_type_nomem.id);
	gen_store_tag(ctx, &qout, expr->result, &qtag);
	pushi(ctx->current, NULL, Q_JMP, &bend, NULL);

	push(&ctx->current->body, &lend);

	return *out;
}

static void
gen_expr_assert(struct gen_context *ctx, const struct expression *expr)
{
	struct qbe_statement failedl, passedl;
	if (expr->assert.cond) {
		struct qbe_value bfailed = mklabel(ctx, &failedl, "failed.%d");
		struct qbe_value bpassed = mklabel(ctx, &passedl, "passed.%d");
		struct gen_value cond = gen_expr(ctx, expr->assert.cond);
		struct qbe_value qcond = mkqval(ctx, &cond);
		pushi(ctx->current, NULL, Q_JNZ, &qcond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
	}

	if (expr->assert.message) {
		struct gen_value msg = gen_expr(ctx, expr->assert.message);
		struct qbe_value path =
			mklval(ctx, &ctx->sources[expr->loc.file]);
		struct qbe_value line = constl(expr->loc.lineno);
		struct qbe_value col = constl(expr->loc.colno);
		struct qbe_value qmsg = mkqval(ctx, &msg);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.abort,
				&path, &line, &col, &qmsg, NULL);
		pushi(ctx->current, NULL, Q_HLT, NULL);
	} else {
		gen_fixed_abort(ctx, expr->loc, expr->assert.fixed_reason);
	}

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}
}

static void
gen_subslice_info(struct gen_context *ctx, const struct expression *expr,
		struct qbe_value *oldlen, struct qbe_value *oldcap,
		struct qbe_value *start, struct qbe_value *end,
		struct qbe_value *newlen, struct qbe_value *newcap)
{
	assert(expr->type == EXPR_SLICE);

	// Callers are allowed to pass NULL for any of these, which tells us
	// they do not care about that value (and sometimes better code can be
	// generated based on that info).
	// The procedure may still need them in some cases internally.
	start = start ? start : &(struct qbe_value){0};
	end = end ? end : &(struct qbe_value){0};
	newlen = newlen ? newlen : &(struct qbe_value){0};
	newcap = newcap ? newcap : &(struct qbe_value){0};

	enum {
		START = 1, END = 1 << 1, LENGTH = 1 << 2
	};
	int bounds = oldlen ? LENGTH : 0;

	if (expr->slice.start) {
		struct gen_value gstart = gen_expr(ctx, expr->slice.start);
		*start = mkqval(ctx, &gstart);
		bounds |= START;
	} else {
		*start = constl(0);
	}
	if (expr->slice.end) {
		struct gen_value gend = gen_expr(ctx, expr->slice.end);
		*end = mkqval(ctx, &gend);
		bounds |= END;
	} else {
		*end = *oldlen;
	}

	*newlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	*newcap = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, newlen, Q_SUB, end, start, NULL);

	struct qbe_value end_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value start_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
	switch (bounds) {
	case START | END | LENGTH:
		pushi(ctx->current, &start_oob, Q_CULEL, start, end, NULL);
		pushi(ctx->current, &end_oob, Q_CULEL, end, oldlen, NULL);
		pushi(ctx->current, &valid, Q_AND, &start_oob, &end_oob, NULL);
		pushi(ctx->current, newlen, Q_SUB, end, start, NULL);
		pushi(ctx->current, newcap, Q_SUB, oldcap, start, NULL);
		break;
	case START | LENGTH:
		pushi(ctx->current, &valid, Q_CULEL, start, oldlen, NULL);
		pushi(ctx->current, newlen, Q_SUB, oldlen, start, NULL);
		pushi(ctx->current, newcap, Q_SUB, oldcap, start, NULL);
		break;
	case END | LENGTH:
		pushi(ctx->current, &valid, Q_CULEL, end, oldlen, NULL);
		pushi(ctx->current, newlen, Q_COPY, end, NULL);
		pushi(ctx->current, newcap, Q_COPY, oldcap, NULL);
		break;
	case START | END:
		pushi(ctx->current, &valid, Q_CULEL, start, end, NULL);
		pushi(ctx->current, newlen, Q_SUB, end, start, NULL);
		pushi(ctx->current, newcap, Q_COPY, newlen, NULL);
		break;
	case LENGTH:
		pushi(ctx->current, newlen, Q_COPY, oldlen, NULL);
		pushi(ctx->current, newcap, Q_COPY, oldcap, NULL);
		return;
	case END:
		pushi(ctx->current, newlen, Q_COPY, end, NULL);
		pushi(ctx->current, newcap, Q_COPY, end, NULL);
		return;
	case START:
	case 0:
		abort();
	}

	struct qbe_statement linvalid, lvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

	pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
	push(&ctx->current->body, &lvalid);
}

static void
gen_expr_assign_slice_expandable(struct gen_context *ctx, struct qbe_value obase,
		struct qbe_value ostart, struct qbe_value olen,
		const struct expression *rvalue)
{
	size_t arrlen = rvalue->result->array.length;
	size_t membsz = rvalue->result->array.members->size;
	struct qbe_value cmplen = constl(arrlen);
	gen_fixed_assert(ctx, rvalue->loc, ABORT_OOB, Q_CULEL, &cmplen, &olen);

	struct qbe_value sz = constl(membsz);
	struct qbe_value off = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct gen_value odata = mkgtemp(ctx, rvalue->result, ".%d");
	struct qbe_value qodata = mkqval(ctx, &odata);
	pushi(ctx->current, &off, Q_MUL, &ostart, &sz, NULL);
	pushi(ctx->current, &qodata, Q_ADD, &obase, &off, NULL);

	gen_expr_at(ctx, rvalue, odata);

	// perform the copy minus the first element
	struct qbe_value loffset = constl(membsz * (arrlen - 1));
	struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &last, Q_ADD, &qodata, &loffset, NULL);
	struct qbe_value noffset = constl(membsz * arrlen);
	struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &next, Q_ADD, &qodata, &noffset, NULL);
	pushi(ctx->current, &olen, Q_MUL, &olen, &sz, NULL);
	pushi(ctx->current, &olen, Q_SUB, &olen, &noffset, NULL);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &olen, NULL);
}

static void
gen_expr_assign_slice(struct gen_context *ctx, const struct expression *expr)
{
	assert(expr->assign.object->type == EXPR_SLICE);

	struct gen_value obj = gen_autoderef_expr(ctx, expr->assign.object->slice.object);
	const struct type *srctype = type_dealias(NULL, obj.type);

	struct qbe_value optr, ostart, olen;
	struct qbe_value oldlen_, *oldlen = &oldlen_, oldcap_, *oldcap = &oldcap_;
	if (srctype->storage == STORAGE_ARRAY) {
		optr = mkcopy(ctx, &obj, ".%d");
		if (srctype->array.length == SIZE_UNDEFINED) {
			oldlen = oldcap = NULL;
		} else {
			*oldlen = *oldcap = constl(srctype->array.length);
		}
	} else {
		struct gen_slice sl = gen_slice_ptrs(ctx, obj);
		load_slice_data(ctx, &sl, &optr, oldlen, oldcap);
	}
	gen_subslice_info(ctx, expr->assign.object, oldlen, oldcap, &ostart, NULL, &olen, NULL);

	const struct type *vtype = type_dealias(NULL, expr->assign.value->result);
	if (vtype->storage == STORAGE_ARRAY && vtype->array.expandable) {
		gen_expr_assign_slice_expandable(ctx, optr, ostart,
			olen, expr->assign.value);
		return;
	}

	struct gen_value val = gen_expr(ctx, expr->assign.value);
	struct qbe_value qval = mkqval(ctx, &val);
	struct qbe_value vlen = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value step = constl(ctx->arch.ptr->size);
	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &ptr, Q_ADD, &qval, &step, NULL);
	pushi(ctx->current, &vlen, Q_LOADL, &ptr, NULL);
	gen_fixed_assert(ctx, expr->loc, ABORT_OOB, Q_CEQL, &olen, &vlen);

	struct qbe_value vptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value os = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value tmp = constl(vtype->array.members->size);
	pushi(ctx->current, &os, Q_MUL, &ostart, &tmp, NULL);
	pushi(ctx->current, &optr, Q_ADD, &optr, &os, NULL);
	pushi(ctx->current, &vptr, Q_LOADL, &qval, NULL);
	pushi(ctx->current, &olen, Q_MUL, &olen, &tmp, NULL);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &optr, &vptr, &olen, NULL);
}

static struct qbe_value
extend(struct gen_context *ctx, struct qbe_value v, const struct type *type)
{
	enum qbe_instr op;
	switch (type->size) {
	case 1:
		op = type_is_signed(NULL, type) ? Q_EXTSB : Q_EXTUB;
		break;
	case 2:
		op = type_is_signed(NULL, type) ? Q_EXTSH : Q_EXTUH;
		break;
	default:
		return v;
	}

	struct qbe_value temp = mkqtmp(ctx, &qbe_word, ".%d");
	pushi(ctx->current, &temp, op, &v, NULL);
	return temp;
}

bool bin_extend[BIN_LAST + 1][2] = {
	[BIN_BAND] = { false, false },
	[BIN_BOR] = { false, false },
	[BIN_DIV] = { true, true },
	[BIN_GREATER] = { true, true },
	[BIN_GREATEREQ] = { true, true },
	[BIN_LAND] = { true, true },
	[BIN_LEQUAL] = { true, true },
	[BIN_LESS] = { true, true },
	[BIN_LESSEQ] = { true, true },
	[BIN_LOR] = { true, true },
	[BIN_LSHIFT] = { false, true },
	[BIN_LXOR] = { true, true },
	[BIN_MINUS] = { true, false },
	[BIN_MODULO] = { true, true },
	[BIN_NEQUAL] = { true, true },
	[BIN_PLUS] = { false, false },
	[BIN_RSHIFT] = { true, true },
	[BIN_TIMES] = { false, false },
	[BIN_BXOR] = { false, false },
};

static void
gen_expr_assign(struct gen_context *ctx, const struct expression *expr)
{
	struct expression *object = expr->assign.object;
	struct expression *value = expr->assign.value;
	if (object->type == EXPR_SLICE) {
		gen_expr_assign_slice(ctx, expr);
		return;
	}

	struct gen_value obj;
	switch (object->type) {
	case EXPR_ACCESS:
		obj = gen_expr_access_addr(ctx, object);
		break;
	case EXPR_UNARITHM:
		assert(object->unarithm.op == UN_DEREF); // Invariant
		obj = gen_expr(ctx, object->unarithm.operand);
		assert(type_dealias(NULL, obj.type)->storage == STORAGE_POINTER);
		obj.type = type_dealias(NULL, obj.type)->pointer.referent;
		break;
	default:
		abort(); // Invariant
	}
	if (value->result->storage == STORAGE_NEVER || value->result->size == 0) {
		gen_expr(ctx, value);
	} else if (expr->assign.op == BIN_LEQUAL) {
		struct gen_value rvalue = gen_expr(ctx, value);
		gen_store(ctx, obj, rvalue);
	} else if (expr->assign.op == BIN_LAND || expr->assign.op == BIN_LOR) {
		struct qbe_statement lrval, lshort;
		struct qbe_value brval = mklabel(ctx, &lrval, ".%d");
		struct qbe_value bshort = mklabel(ctx, &lshort, ".%d");
		struct gen_value load = gen_load(ctx, obj);
		struct qbe_value qload = mkqval(ctx, &load);
		if (expr->binarithm.op == BIN_LAND) {
			pushi(ctx->current, NULL, Q_JNZ, &qload, &brval,
				&bshort, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ, &qload, &bshort,
				&brval, NULL);
		}
		push(&ctx->current->body, &lrval);
		gen_expr_at(ctx, value, obj);
		pushi(ctx->current, NULL, Q_JMP, &bshort, NULL);
		push(&ctx->current->body, &lshort);
	} else {
		struct gen_value lvalue = gen_load(ctx, obj);
		struct gen_value rvalue = gen_expr(ctx, value);
		struct qbe_value qlval = mkqval(ctx, &lvalue);
		struct qbe_value ilval = qlval;
		struct qbe_value qrval = mkqval(ctx, &rvalue);
		enum qbe_instr instr = binarithm_for_op(ctx,
			expr->assign.op, lvalue.type);
		if (bin_extend[expr->assign.op][1]) {
			qrval = extend(ctx, qrval, rvalue.type);
		}
		pushi(ctx->current, &qlval, instr, &ilval, &qrval, NULL);
		gen_store(ctx, obj, lvalue);
	}
}

static struct gen_value
gen_expr_binarithm_gv(struct gen_context *ctx, const struct type *result,
		const struct type *ltype, const struct type *rtype, int op,
		struct gen_value lvalue, struct gen_value rvalue)
{
	struct qbe_value qlval = mkqval(ctx, &lvalue);
	struct qbe_value qrval = mkqval(ctx, &rvalue);
	if (bin_extend[op][0]) {
		qlval = extend(ctx, qlval, ltype);
	}
	if (bin_extend[op][1]) {
		qrval = extend(ctx, qrval, rtype);
	}

	struct gen_value gresult = mkgtemp(ctx, result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &gresult);
	ltype = type_dealias(NULL, ltype);
	rtype = type_dealias(NULL, rtype);
	assert((ltype->storage == STORAGE_STRING) == (rtype->storage == STORAGE_STRING));
	if (ltype->storage == STORAGE_STRING) {
		pushi(ctx->current, &qresult, Q_CALL,
			&ctx->rt.strcmp, &qlval, &qrval, NULL);
		if (op == BIN_NEQUAL) {
			struct qbe_value one = constl(1);
			pushi(ctx->current, &qresult, Q_XOR, &qresult, &one, NULL);
		} else {
			assert(op == BIN_LEQUAL);
		}
		return gresult;
	}
	enum qbe_instr instr = binarithm_for_op(ctx, op, ltype);
	pushi(ctx->current, &qresult, instr, &qlval, &qrval, NULL);
	return gresult;
}

static struct gen_value
gen_expr_binarithm(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *ltype = type_dealias(NULL, expr->binarithm.lvalue->result);
	const struct type *rtype = type_dealias(NULL, expr->binarithm.rvalue->result);
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);

	if (expr->binarithm.op == BIN_LAND || expr->binarithm.op == BIN_LOR) {
		struct qbe_statement lrval, lshort;
		struct qbe_value brval = mklabel(ctx, &lrval, ".%d");
		struct qbe_value bshort = mklabel(ctx, &lshort, ".%d");
		struct gen_value lval = gen_expr(ctx, expr->binarithm.lvalue);
		struct qbe_value qlval = mkqval(ctx, &lval);
		pushi(ctx->current, &qresult, Q_COPY, &qlval, NULL);
		if (expr->binarithm.op == BIN_LAND) {
			pushi(ctx->current, NULL, Q_JNZ, &qresult, &brval,
				&bshort, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ, &qresult, &bshort,
				&brval, NULL);
		}
		push(&ctx->current->body, &lrval);
		struct gen_value rval = gen_expr(ctx, expr->binarithm.rvalue);
		struct qbe_value qrval = mkqval(ctx, &rval);
		pushi(ctx->current, &qresult, Q_COPY, &qrval, NULL);
		if (rtype->storage != STORAGE_NEVER) {
			pushi(ctx->current, NULL, Q_JMP, &bshort, NULL);
		}
		push(&ctx->current->body, &lshort);
		return result;
	}
	struct gen_value lvalue = gen_expr(ctx, expr->binarithm.lvalue);
	struct gen_value rvalue = gen_expr(ctx, expr->binarithm.rvalue);
	return gen_expr_binarithm_gv(ctx, expr->result, ltype, rtype,
			expr->binarithm.op, lvalue, rvalue);
}

static void
gen_expr_binding_unpack_static(struct gen_context *ctx,
	const struct expression_binding *binding)
{
	assert(binding->object == NULL);

	struct tuple_literal *tuplelit =
		binding->initializer->literal.tuple;

	for (const struct binding_unpack *unpack = binding->unpack;
			unpack; unpack = unpack->next) {
		if (unpack->object == NULL) {
			goto done;
		}
		assert(unpack->object->otype == O_DECL);

		struct declaration decl = {
			.decl_type = DECL_GLOBAL,
			.ident = unpack->object->ident,
			.global = {
				.type = unpack->object->type,
				.value = tuplelit->value,
			},
		};
		gen_global_decl(ctx, &decl);

done:
		tuplelit = tuplelit->next;
	}
}

static void
gen_expr_binding_unpack(struct gen_context *ctx,
	const struct expression_binding *binding)
{
	assert(binding->object == NULL);

	const struct type *type = binding->initializer->result;
	struct gen_value tuple_gv = mkgtemp(ctx, type, "tupleunpack.%d");
	struct qbe_value tuple_qv = mklval(ctx, &tuple_gv);
	struct qbe_value sz = constl(type->size);
	enum qbe_instr alloc = alloc_for_align(type->align);
	pushprei(ctx->current, &tuple_qv, alloc, &sz, NULL);

	gen_expr_at(ctx, binding->initializer, tuple_gv);

	for (const struct binding_unpack *unpack = binding->unpack;
			unpack; unpack = unpack->next) {
		if (unpack->object == NULL) {
			continue;
		}
		assert(unpack->object->otype != O_DECL);

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value = mkgtemp(ctx, unpack->object->type, "binding.%d");
		gb->object = unpack->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;
		struct qbe_value item_qv = mklval(ctx, &gb->value);
		struct qbe_value offs = constl(unpack->offset);
		pushprei(ctx->current, &item_qv, Q_ADD, &tuple_qv, &offs, NULL);
	}
}

static void
gen_expr_binding(struct gen_context *ctx, const struct expression *expr)
{
	for (const struct expression_binding *binding = &expr->binding;
			binding; binding = binding->next) {
		if (binding->unpack) {
			if (binding->unpack->object->otype == O_DECL) {
				gen_expr_binding_unpack_static(ctx, binding);
			} else {
				gen_expr_binding_unpack(ctx, binding);
			}
			continue;
		}

		if (binding->object->otype == O_DECL) {
			// static binding
			struct declaration decl = {
				.decl_type = DECL_GLOBAL,
				.ident = binding->object->ident,
				.global = {
					.type = binding->object->type,
					.value = binding->initializer,
				},
			};
			gen_global_decl(ctx, &decl);
			continue;
		}

		const struct type *type = binding->object->type;
		if (type->size == 0) {
			gen_expr(ctx, binding->initializer);
			continue;
		}

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value = mkgtemp(ctx, type, "binding.%d");
		gb->object = binding->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		struct qbe_value qv = mklval(ctx, &gb->value);
		struct qbe_value sz = constl(type->size);
		enum qbe_instr alloc = alloc_for_align(type->align);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);
		gen_expr_at(ctx, binding->initializer, gb->value);
	}
}

static void
gen_expr_control(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_scope *scope = gen_scope_lookup(ctx, expr->control.scope);

	if (expr->control.value) {
		gen_expr_branch(ctx, expr->control.value, scope->result, scope->out);
		if (expr->control.value->result->storage == STORAGE_NEVER) {
			return;
		}
	}

	struct gen_scope *deferred = ctx->scope;
	while (deferred != NULL) {
		gen_defers(ctx, deferred);
		if (deferred == scope) {
			break;
		}
		deferred = deferred->parent;
	}

	switch (expr->type) {
	case EXPR_BREAK:
		assert(scope->scope->class == SCOPE_LOOP);
		pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
		break;
	case EXPR_CONTINUE:
		assert(scope->scope->class == SCOPE_LOOP);
		pushi(ctx->current, NULL, Q_JMP, scope->after, NULL);
		break;
	case EXPR_YIELD:
		assert(scope->scope->class == SCOPE_COMPOUND);
		pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
		break;
	default: abort(); // Invariant
	}
}

static struct gen_value
gen_expr_call(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value lvalue = gen_autoderef_expr(ctx, expr->call.lvalue);
	const struct type *rtype = type_dealias(NULL, lvalue.type);
	assert(rtype->storage == STORAGE_FUNCTION);

	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};
	struct gen_value rval = gv_void;
	if (rtype->func.result->size != 0
			&& rtype->func.result->storage != STORAGE_NEVER) {
		rval = mkgtemp(ctx, rtype->func.result, ".%d");
		call.out = xcalloc(1, sizeof(struct qbe_value));
		*call.out = mkqval(ctx, &rval);
		call.out->type = qtype_lookup(ctx, rtype->func.result, false);
	}

	bool cvar = false;
	struct type_func_param *param = rtype->func.params;
	struct qbe_arguments *args, **next = &call.args;
	args = *next = xcalloc(1, sizeof(struct qbe_arguments));
	args->value = mkqval(ctx, &lvalue);
	next = &args->next;
	for (struct call_argument *carg = expr->call.args; ; carg = carg->next) {
		if (!param && !cvar && rtype->func.variadism == VARIADISM_C) {
			cvar = true;
			args = *next = xcalloc(1, sizeof(struct qbe_arguments));
			args->value.kind = QV_VARIADIC;
			next = &args->next;
		}
		if (!carg) {
			break;
		}

		struct gen_value arg = gen_expr(ctx, carg->value);
		if (param) {
			param = param->next;
		}
		if (carg->value->result->size == 0) {
			continue;
		}
		args = *next = xcalloc(1, sizeof(struct qbe_arguments));
		if (carg->value->result->storage == STORAGE_NEVER) {
			return gv_void;
		}
		args->value = mkqval(ctx, &arg);
		args->value.type = qtype_lookup(ctx, carg->value->result, true);
		next = &args->next;
	}

	if (rtype->func.result->storage == STORAGE_NEVER) {
		for (struct gen_scope *scope = ctx->scope; scope;
				scope = scope->parent) {
			gen_defers(ctx, scope);
			if (scope->scope->class == SCOPE_DEFER) {
				break;
			}
		}
	}

	push(&ctx->current->body, &call);
	if (rtype->func.result->storage == STORAGE_NEVER) {
		pushi(ctx->current, NULL, Q_HLT, NULL);
	}

	return rval;
}

static struct gen_value gen_expr_cast(struct gen_context *ctx,
		const struct expression *expr);

static struct gen_value gen_subset_match_tests(struct gen_context *ctx,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type);

static struct gen_value gen_nested_match_tests(struct gen_context *ctx,
		struct gen_value object, struct qbe_value bmatch,
		struct qbe_value bnext, struct qbe_value tag,
		const struct type *type, const struct type *subtype);

static void
gen_type_assertion_at(struct gen_context *ctx, const struct expression *expr,
	struct gen_value base)
{
	gen_expr_at(ctx, expr->cast.value, base);
	assert(expr->cast.kind == C_ASSERTION);
	const struct type *want = expr->cast.secondary;
	struct qbe_value tag = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value qbase = mkqval(ctx, &base);
	gen_load_tag(ctx, &tag, &qbase, expr->cast.value->result);

	struct qbe_statement failedl, passedl;
	struct qbe_value bfailed, bpassed;
	bpassed = mklabel(ctx, &passedl, "passed.%d");
	bfailed = mklabel(ctx, &failedl, "failed.%d");

	if (tagged_select_subtype(NULL, expr->cast.value->result, want, true)) {
		gen_nested_match_tests(ctx, base, bpassed,
				bfailed, tag, expr->cast.value->result, want);
	} else if (tagged_subset_compat(NULL, expr->cast.value->result, want)) {
		gen_subset_match_tests(ctx, bpassed, bfailed, tag,
				type_dealias(NULL, want));
	} else {
		abort();
	}

	push(&ctx->current->body, &failedl);
	gen_fixed_abort(ctx, expr->loc, ABORT_TYPE_ASSERTION);
	push(&ctx->current->body, &passedl);
}

static struct gen_value
gen_type_test(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value base = gen_expr(ctx, expr->cast.value);

	assert(expr->cast.kind == C_TEST);
	const struct type *want = expr->cast.secondary;
	struct qbe_value tag = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value qbase = mkqval(ctx, &base);
	gen_load_tag(ctx, &tag, &qbase, expr->cast.value->result);

	struct qbe_statement dummy;
	struct qbe_value bdummy = mklabel(ctx, &dummy, "failed.%d");

	struct gen_value result;
	if (tagged_select_subtype(NULL, expr->cast.value->result, want, true)) {
		result = gen_nested_match_tests(ctx, base, bdummy,
				bdummy, tag, expr->cast.value->result, want);
	} else if (tagged_subset_compat(NULL, expr->cast.value->result, want)) {
		result = gen_subset_match_tests(ctx, bdummy, bdummy, tag,
				type_dealias(NULL, want));
	} else {
		abort();
	}
	push(&ctx->current->body, &dummy);
	return result;
}

static void
gen_expr_cast_slice_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *from = type_dealias(NULL, expr->cast.value->result);
	if (from->storage == STORAGE_POINTER) {
		from = type_dealias(NULL, from->pointer.referent);
	}
	assert(from->storage == STORAGE_ARRAY);
	assert(from->array.length != SIZE_UNDEFINED);

	struct qbe_value data;
	struct gen_value value = gen_expr(ctx, expr->cast.value);
	if (from->array.length == 0) {
		data = constl(0);
	} else {
		data = mkqval(ctx, &value);
	}
	struct qbe_value ln = constl(from->array.length);
	struct gen_slice sl = gen_slice_ptrs(ctx, out);
	store_slice_data(ctx, &sl, &data, &ln, &ln);
}

static void
gen_expr_cast_tagged_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	// Generate a cast from type "from" to type "to". Two cases are handled
	// here:
	//
	// 1. "from" is a tagged union which is compatible with "to"
	//    let from: (i32 | void);
	//    let to: (i32 | i64 | void) = from;
	// 2. "from" is a member of "to"
	//    let from: i32;
	//    let to: (i32 | void) = from;
	assert(expr->type == EXPR_CAST);
	const struct type *to = expr->result, *from = expr->cast.value->result;
	const struct type *subtype = tagged_select_subtype(NULL, to, from, true);

	if (!subtype) {
		// Compatible tagged unions
		if (expr->cast.kind == C_ASSERTION) {
			gen_type_assertion_at(ctx, expr, out);
		} else {
			gen_expr_at(ctx, expr->cast.value, out);
		}
	} else {
		// "from" is a member of "to"
		//
		// Update the tag and generate the "from" expression at the
		// offset of the value field.
		struct qbe_value qout = mkqval(ctx, &out);
		struct qbe_value id = constw(subtype->id);
		gen_store_tag(ctx, &qout, to, &id);

		if (subtype->size == 0) {
			gen_expr(ctx, expr->cast.value); // side-effects
			return;
		}

		struct gen_value storage = mkgtemp(ctx, subtype, ".%d");
		struct qbe_value qstor = mklval(ctx, &storage);
		struct qbe_value offs = compute_tagged_memb_offset(from);
		pushi(ctx->current, &qstor, Q_ADD, &qout, &offs, NULL);
		gen_expr_at(ctx, expr->cast.value, storage);
	}
}

static bool
cast_prefers_at(const struct expression *expr)
{
	const struct type *to = expr->result, *from = expr->cast.value->result;
	if (expr->cast.kind == C_TEST) {
		return false;
	}
	// tagged => *; subtype compatible
	if (type_dealias(NULL, from)->storage == STORAGE_TAGGED
			&& tagged_select_subtype(NULL, from, to, true)) {
		return false;
	}
	// * => tagged
	if (type_dealias(NULL, to)->storage == STORAGE_TAGGED) {
		return true;
	}
	// array => array
	if (type_dealias(NULL, to)->storage == STORAGE_ARRAY
			&& type_dealias(NULL, from)->storage == STORAGE_ARRAY) {
		return true;
	}
	// array => slice
	if (type_dealias(NULL, to)->storage == STORAGE_SLICE) {
		switch (type_dealias(NULL, from)->storage) {
		case STORAGE_ARRAY:
			return true;
		case STORAGE_POINTER:
			from = type_dealias(NULL, from)->pointer.referent;
			return type_dealias(NULL, from)->storage == STORAGE_ARRAY;
		default:
			return false;
		}
	}
	return false;
}

static void
gen_expr_cast_array_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *typeout = type_dealias(NULL, expr->result);
	const struct type *typein = type_dealias(NULL, expr->cast.value->result);
	gen_expr_at(ctx, expr->cast.value, out);
	if (!typein->array.expandable) {
		return;
	}

	assert(typein->array.length != SIZE_UNDEFINED
			&& typeout->array.length != SIZE_UNDEFINED);
	assert(typeout->array.length >= typein->array.length);

	const struct type *membtype = typein->array.members;
	size_t remain = typeout->array.length - typein->array.length;

	struct qbe_value base = mkqval(ctx, &out);
	struct qbe_value offs = constl((typein->array.length - 1) * membtype->size);
	struct gen_value next = mkgtemp(ctx, membtype, ".%d");
	struct qbe_value ptr = mklval(ctx, &next);
	struct gen_value item = mkgtemp(ctx, membtype, "item.%d");
	struct qbe_value qitem = mklval(ctx, &item);
	pushi(ctx->current, &qitem, Q_ADD, &base, &offs, NULL);

	if (remain * membtype->size <= 128) {
		struct gen_value last = gen_load(ctx, item);
		for (size_t n = typein->array.length; n < typeout->array.length; ++n) {
			struct qbe_value offs = constl(n * membtype->size);
			pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
			gen_store(ctx, next, last);
		}
		return;
	}

	offs = constl(typein->array.length * membtype->size);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);

	struct qbe_value dtemp = mklval(ctx, &next);
	struct qbe_value stemp = mklval(ctx, &item);
	struct qbe_value sz = constl(remain * membtype->size);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &dtemp, &stemp, &sz, NULL);
}

static void
gen_expr_cast_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	if (!cast_prefers_at(expr)) {
		struct gen_value result = gen_expr_cast(ctx, expr);
		gen_store(ctx, out, result);
		return;
	}

	if (expr->cast.lowered) {
		pushc(ctx->current, "gen lowered cast");
	}

	const struct type *to = expr->result;
	switch (type_dealias(NULL, to)->storage) {
	case STORAGE_SLICE:
		gen_expr_cast_slice_at(ctx, expr, out);
		break;
	case STORAGE_TAGGED:
		gen_expr_cast_tagged_at(ctx, expr, out);
		break;
	case STORAGE_ARRAY:
		gen_expr_cast_array_at(ctx, expr, out);
		break;
	default: abort(); // Invariant
	}
}

static struct qbe_value nested_tagged_offset(const struct type *tu,
		const struct type *targed);

static struct gen_value
gen_expr_cast(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *to = expr->cast.secondary,
		*from = expr->cast.value->result;
	if (expr->cast.kind != C_CAST) {
		bool is_valid_tagged, is_valid_pointer;
		is_valid_tagged = type_dealias(NULL, from)->storage == STORAGE_TAGGED
				&& (tagged_select_subtype(NULL, from, to, true)
				|| tagged_subset_compat(NULL, from, to));
		is_valid_pointer = type_dealias(NULL, from)->storage == STORAGE_POINTER
				&& (type_dealias(NULL, to)->storage == STORAGE_POINTER
				|| type_dealias(NULL, to)->storage == STORAGE_NULL);
		assert(is_valid_tagged || is_valid_pointer);
		if (expr->cast.kind == C_TEST && is_valid_tagged) {
			return gen_type_test(ctx, expr);
		}
	}

	if (cast_prefers_at(expr)) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_cast_at(ctx, expr, out);
		return out;
	}

	if (expr->cast.lowered) {
		pushc(ctx->current, "gen lowered cast");
	}

	// Special cases
	bool want_null = false;
	switch (type_dealias(NULL, to)->storage) {
	case STORAGE_NULL:
		want_null = true;
		// fallthrough
	case STORAGE_POINTER:
		if (type_dealias(NULL, from)->storage == STORAGE_SLICE) {
			struct gen_value value = gen_expr(ctx, expr->cast.value);
			value.type = to;
			return gen_load(ctx, value);
		}
		if (type_dealias(NULL, from)->storage != STORAGE_POINTER) {
			break;
		}

		struct gen_value val = gen_expr(ctx, expr->cast.value);
		struct qbe_value qval = mkqval(ctx, &val);
		struct qbe_value zero = constl(0);
		enum qbe_instr compare = want_null ? Q_CEQL : Q_CNEL;
		if (expr->cast.kind == C_TEST) {
			struct gen_value out =
				mkgtemp(ctx, &builtin_type_bool, ".%d");
			struct qbe_value qout = mkqval(ctx, &out);

			pushi(ctx->current, &qout, compare, &qval, &zero, NULL);
			return out;
		} else if (expr->cast.kind == C_ASSERTION) {
			gen_fixed_assert(ctx, expr->loc, ABORT_TYPE_ASSERTION, compare, &qval, &zero);
			if (want_null) {
				return (struct gen_value){
					.kind = GV_CONST,
					.type = &builtin_type_null,
					.lval = 0,
				};
			}
		}
		val.type = to;
		return val;
	default: break;
	}

	// Special case: tagged => non-tagged
	if (type_dealias(NULL, from)->storage == STORAGE_TAGGED) {
		struct gen_value gbase;
		if (expr->cast.kind == C_ASSERTION) {
			gbase = mkgtemp(ctx, expr->cast.value->result, ".%d");
			struct qbe_value qbase = mkqval(ctx, &gbase);
			enum qbe_instr alloc =
				alloc_for_align(expr->cast.value->result->align);
			struct qbe_value size =
				constl(expr->cast.value->result->size);
			pushprei(ctx->current, &qbase, alloc, &size, NULL);
			gen_type_assertion_at(ctx, expr, gbase);
		} else {
			gbase = gen_expr(ctx, expr->cast.value);
		}
		struct qbe_value base = mkcopy(ctx, &gbase, ".%d");

		if (type_dealias(NULL, to)->size == 0) {
			return gv_void;
		}

		struct qbe_value align = nested_tagged_offset(
				expr->cast.value->result, expr->cast.secondary);
		pushi(ctx->current, &base, Q_ADD, &base, &align, NULL);
		struct gen_value storage = (struct gen_value){
			.kind = GV_TEMP,
			.type = to,
			.name = base.name,
		};
		return gen_load(ctx, storage);
	}

	// Special case: cast to type with size zero
	if (type_dealias(NULL, to)->size == 0) {
		gen_expr(ctx, expr->cast.value); // Side-effects
		return gv_void;
	}

	// Special case: no conversion required
	if (type_dealias(NULL, to)->storage == type_dealias(NULL, from)->storage
			&& to->size == from->size) {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		value.type = to;
		return value;
	}

	struct gen_value value = gen_expr(ctx, expr->cast.value);
	struct qbe_value qvalue = mkqval(ctx, &value);
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	struct gen_value intermediate;
	struct qbe_value qintermediate;

	from = lower_flexible(NULL, from, NULL);

	enum qbe_instr op;
	bool is_signed = type_is_signed(NULL, from);
	enum type_storage fstor = type_dealias(NULL, from)->storage,
		tstor = type_dealias(NULL, to)->storage;
	switch (tstor) {
	case STORAGE_ENUM:
	case STORAGE_U8:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_U16:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_UINTPTR:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
		if (type_is_integer(NULL, from) || fstor == STORAGE_RUNE) {
			if (to->size <= from->size) {
				op = Q_COPY;
			} else {
				switch (from->size) {
				case 4:
					op = is_signed ? Q_EXTSW : Q_EXTUW;
					break;
				case 2:
					op = is_signed ? Q_EXTSH : Q_EXTUH;
					break;
				case 1:
					op = is_signed ? Q_EXTSB : Q_EXTUB;
					break;
				default:
					assert(0); // Invariant
				}
			}
		} else if (fstor == STORAGE_POINTER || fstor == STORAGE_NULL) {
			assert(tstor == STORAGE_UINTPTR);
			op = Q_COPY;
		} else if (type_is_float(NULL, from)) {
			if (type_is_signed(NULL, to)) {
				switch (fstor) {
				case STORAGE_F32: op = Q_STOSI; break;
				case STORAGE_F64: op = Q_DTOSI; break;
				default: abort(); // Invariant
				}
			} else {
				switch (fstor) {
				case STORAGE_F32: op = Q_STOUI; break;
				case STORAGE_F64: op = Q_DTOUI; break;
				default: abort(); // Invariant
				}
			}
		} else {
			abort(); // Invariant
		}
		pushi(ctx->current, &qresult, op, &qvalue, NULL);
		break;
	case STORAGE_F32:
	case STORAGE_F64:
		if (type_is_float(NULL, from) && from->size == to->size) {
			op = Q_COPY;
		} else if (type_is_float(NULL, from) && to->size < from->size) {
			op = Q_TRUNCD;
		} else if (type_is_float(NULL, from) && to->size > from->size) {
			op = Q_EXTS;
		} else if (type_is_integer(NULL, from)) {
			if (type_is_signed(NULL, from)) {
				switch (from->size) {
				case 1:
				case 2:
					intermediate = mkgtemp(ctx,
						&builtin_type_i32, ".%d");
					qintermediate = mkqval(ctx, &intermediate);
					pushi(ctx->current, &qintermediate,
						from->size == 1? Q_EXTSB : Q_EXTSH,
						&qvalue, NULL);
					qvalue = qintermediate;
					/* fallthrough */
				case 4:
					op = Q_SWTOF;
					break;
				case 8:
					op = Q_SLTOF;
					break;
				default: abort(); // Invariant
				}
			} else {
				switch (from->size) {
				case 1:
				case 2:
					intermediate = mkgtemp(ctx,
						&builtin_type_i32, ".%d");
					qintermediate = mkqval(ctx, &intermediate);
					pushi(ctx->current, &qintermediate,
						from->size == 1? Q_EXTUB : Q_EXTUH,
						&qvalue, NULL);
					qvalue = qintermediate;
					/* fallthrough */
				case 4:
					op = Q_UWTOF;
					break;
				case 8:
					op = Q_ULTOF;
					break;
				default: abort(); // Invariant
				}
			}
		} else {
			abort(); // Invariant
		}
		pushi(ctx->current, &qresult, op, &qvalue, NULL);
		break;
	case STORAGE_NULL:
	case STORAGE_POINTER:
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_ARRAY:
		assert(from->storage == STORAGE_ARRAY);
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_SLICE:
		assert(from->storage == STORAGE_SLICE);
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_ALIAS:
	case STORAGE_BOOL:
	case STORAGE_DONE:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NEVER:
	case STORAGE_NOMEM:
	case STORAGE_OPAQUE:
	case STORAGE_RCONST:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		abort(); // Invariant
	}

	return result;
}

static struct gen_value
gen_expr_compound_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct qbe_statement lend;
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct gen_scope *scope = push_scope(ctx, expr->compound.scope);
	scope->end = &bend;

	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}
	scope->out = out;
	scope->result = gvout;

	const struct expressions *exprs;
	for (exprs = &expr->compound.exprs; exprs->next; exprs = exprs->next) {
		gen_expr(ctx, exprs->expr);
	}

	gen_expr_branch(ctx, exprs->expr, gvout, out);
	pop_scope(ctx);
	push(&ctx->current->body, &lend);
	return gvout;
}

static void
gen_literal_array_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct array_literal *aexpr = expr->literal.array;
	struct qbe_value base = mkqval(ctx, &out);

	size_t n = 0;
	const struct type *atype = type_dealias(NULL, expr->result);
	size_t msize = atype->array.members->size;
	struct gen_value item = mkgtemp(ctx, atype->array.members, "item.%d");
	struct qbe_value ptr;
	for (const struct array_literal *ac = aexpr; ac; ac = ac->next) {
		struct qbe_value offs = constl(n * msize);
		ptr = mklval(ctx, &item);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, ac->value, item);
		++n;
	}
	assert(n == atype->array.length);
	if (!atype->array.expandable || n == 0) {
		return;
	}
	assert(out.type);
	const struct type_array arr = type_dealias(NULL, out.type)->array;
	if (arr.length <= n) {
		return;
	}

	struct qbe_value nsize = constl(n * msize);
	struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &next, Q_ADD, &base, &nsize, NULL);

	struct qbe_value qlen = constl((arr.length - n) * msize);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &ptr, &qlen, NULL);
}

static void
gen_literal_slice_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct array_literal *aexpr = expr->literal.slice.array;
	struct qbe_value obj;
	if (expr->literal.object == NULL && aexpr != NULL) {
		// slicing a literal array

		struct expression *first = aexpr->value;
		obj = mkqtmp(ctx, ctx->arch.ptr, "object.%d");

		size_t n = 0;
		struct gen_value item = mkgtemp(ctx, first->result, "item.%d");
		struct qbe_value ptr;
		for (const struct array_literal *ac = aexpr; ac; ac = ac->next) {
			struct qbe_value offs = constl(n * first->result->size);
			ptr = mklval(ctx, &item);
			pushi(ctx->current, &ptr, Q_ADD, &obj, &offs, NULL);
			gen_expr_at(ctx, ac->value, item);
			++n;
		}
		struct qbe_value asz = constl(n * first->result->size);
		enum qbe_instr alloc = alloc_for_align(first->result->align);
		pushprei(ctx->current, &obj, alloc, &asz, NULL);

		struct qbe_value offset =
			constl(expr->literal.slice.start * first->result->size);
		pushi(ctx->current, &obj, Q_ADD, &obj, &offset, NULL);
	} else if (expr->literal.object == NULL) {
		// slicing an empty array
		obj = constl(0);
	} else {
		// slicing an access expression

		const struct scope_object *sobj = expr->literal.object;
		struct gen_value gobj = gen_access_ident(ctx, sobj);
		struct qbe_value tmp = mkqval(ctx, &gobj);
		const struct type *otype =
			sobj->otype == O_CONST ? sobj->value->result : sobj->type;
		size_t offs = expr->literal.slice.offset;
		offs += expr->literal.slice.start * otype->size;
		struct qbe_value qoffs = constl(offs);
		obj = mkqtmp(ctx, ctx->arch.ptr, "object.%d");
		pushi(ctx->current, &obj, Q_ADD, &tmp, &qoffs, NULL);
	}

	struct gen_slice sl = gen_slice_ptrs(ctx, out);
	struct qbe_value len = constl(expr->literal.slice.len);
	struct qbe_value cap = constl(expr->literal.slice.cap);
	store_slice_data(ctx, &sl, &obj, &len, &cap);
}

static struct qbe_data_item *gen_data_item(struct gen_context *,
	const struct expression *, struct qbe_data_item *);

static struct gen_value
gen_literal_string(struct gen_context *ctx, const struct expression *expr)
{
	struct qbe_def *s = xcalloc(1, sizeof(struct qbe_def));
	s->kind = Q_DATA;
	s->data.align = ALIGN_UNDEFINED;
	s->exported = false;
	s->name = gen_name(&ctx->id, "strliteral.%d");
	s->file = expr->loc.file;
	gen_data_item(ctx, expr, &s->data.items);
	qbe_append_def(ctx->out, s);

	return (struct gen_value){
		.kind = GV_GLOBAL,
		.type = expr->result,
		.name = xstrdup(s->name),
	};
}

static void
gen_literal_struct_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	// TODO: Merge me into literal expressions
	struct qbe_value base = mkqval(ctx, &out);

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct struct_literal *field = expr->literal._struct;
			field; field = field->next) {
		assert(field->value);

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static void
gen_literal_tagged_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	struct qbe_value qout = mklval(ctx, &out);
	const struct type *subtype = expr->literal.tagged.tag;
	struct qbe_value id = constw(subtype->id);
	gen_store_tag(ctx, &qout, expr->result, &id);

	if (subtype->size == 0) {
		return;
	}

	struct gen_value storage = mkgtemp(ctx, subtype, ".%d");
	struct qbe_value qstor = mklval(ctx, &storage);
	struct qbe_value offs = compute_tagged_memb_offset(subtype);
	pushi(ctx->current, &qstor, Q_ADD, &qout, &offs, NULL);
	gen_expr_at(ctx, expr->literal.tagged.value, storage);
}

static void
gen_literal_tuple_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	// TODO: Merge me into literal expressions
	struct qbe_value base = mkqval(ctx, &out);

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct tuple_literal *field = expr->literal.tuple; field;
			field = field->next) {
		assert(field->value);

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static void
gen_expr_literal_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	if (!type_is_aggregate(type_dealias(NULL, expr->result))) {
		struct gen_value val = gen_expr(ctx, expr);
		gen_store(ctx, out, val);
		return;
	}

	switch (type_dealias(NULL, expr->result)->storage) {
	case STORAGE_ARRAY:
		gen_literal_array_at(ctx, expr, out);
		break;
	case STORAGE_SLICE:
		gen_literal_slice_at(ctx, expr, out);
		break;
	case STORAGE_STRUCT:
		gen_literal_struct_at(ctx, expr, out);
		break;
	case STORAGE_TAGGED:
		gen_literal_tagged_at(ctx, expr, out);
		break;
	case STORAGE_TUPLE:
		gen_literal_tuple_at(ctx, expr, out);
		break;
	default:
		gen_store(ctx, out, gen_expr(ctx, expr));
	}
}

static struct gen_value
gen_expr_literal(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value val = {
		.kind = GV_CONST,
		.type = expr->result,
	};

	// Special cases
	switch (type_dealias(NULL, expr->result)->storage) {
	case STORAGE_BOOL:
		val.wval = expr->literal.bval ? 1 : 0;
		return val;
	case STORAGE_NULL:
		val.lval = 0;
		return val;
	case STORAGE_STRING:
		return gen_literal_string(ctx, expr);
	default:
		if (expr->result->size == 0) {
			return gv_void;
		}
		// Moving right along
		break;
	}

	if (type_is_aggregate(type_dealias(NULL, expr->result))) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_at(ctx, expr, out);
		return out;
	}

	if (expr->literal.object != NULL) {
		assert(expr->literal.ival == 0);
		val = gen_access_ident(ctx, expr->literal.object);
		val.type = expr->result;
		return val;
	}

	const struct qbe_type *qtype = qtype_lookup(ctx, expr->result, false);
	switch (qtype->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		val.wval = (uint32_t)expr->literal.uval;
		return val;
	case Q_LONG:
		val.lval = expr->literal.uval;
		return val;
	case Q_SINGLE:
		pushc(ctx->current, "%f", (float)expr->literal.fval);
		val.sval = (float)expr->literal.fval;
		return val;
	case Q_DOUBLE:
		pushc(ctx->current, "%f", expr->literal.fval);
		val.dval = expr->literal.fval;
		return val;
	case Q__VOID:
	case Q__AGGREGATE:
	case Q__UNION:
		assert(0); // Invariant
	}

	abort(); // Invariant
}

static void
gen_expr_defer(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_defer *defer = xcalloc(1, sizeof(struct gen_defer));
	defer->expr = expr;
	defer->next = ctx->scope->defers;
	ctx->scope->defers = defer;
}

static void
gen_expr_delete(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value object;
	struct qbe_value qstart;
	const struct expression *dexpr = expr->delete.expr;
	if (dexpr->type == EXPR_SLICE) {
		object = gen_autoderef_expr(ctx, dexpr->slice.object);
	} else {
		assert(dexpr->type == EXPR_ACCESS
			&& dexpr->access.type == ACCESS_INDEX);
		object = gen_autoderef_expr(ctx, dexpr->access.array);
		struct gen_value start = gen_expr(ctx, dexpr->access.index);
		qstart = mkqval(ctx, &start);
	}
	assert(type_dealias(NULL, object.type)->storage == STORAGE_SLICE);

	struct qbe_value data, qlen, qcap;
	struct gen_slice sl = gen_slice_ptrs(ctx, object);
	load_slice_data(ctx, &sl, &data, &qlen, &qcap);

	struct qbe_value qend;
	if (dexpr->type == EXPR_SLICE) {
		gen_subslice_info(ctx, dexpr, &qlen, &qcap, &qstart, &qend, NULL, NULL);
	} else {
		gen_fixed_assert(ctx, expr->loc, ABORT_OOB, Q_CULTL, &qstart, &qlen);
		struct qbe_value tmp = constl(1);
		qend = mkqtmp(ctx, qstart.type, ".%d");
		pushi(ctx->current, &qend, Q_ADD, &qstart, &tmp, NULL);
	}

	struct qbe_value startptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value endptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value mlen = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value membsz =
		constl(type_dealias(NULL, object.type)->array.members->size);
	pushi(ctx->current, &startptr, Q_MUL, &qstart, &membsz, NULL);
	pushi(ctx->current, &startptr, Q_ADD, &startptr, &data, NULL);
	pushi(ctx->current, &endptr, Q_MUL, &qend, &membsz, NULL);
	pushi(ctx->current, &endptr, Q_ADD, &endptr, &data, NULL);
	pushi(ctx->current, &qlen, Q_SUB, &qlen, &qend, NULL);
	pushi(ctx->current, &mlen, Q_MUL, &qlen, &membsz, NULL);

	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &startptr, &endptr, &mlen,
		NULL);

	pushi(ctx->current, &qlen, Q_ADD, &qlen, &qstart, NULL);
	store_slice_data(ctx, &sl, NULL, &qlen, NULL);

	if (!expr->delete.is_static) {
		struct qbe_value qobj = mklval(ctx, &object);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.unensure, &qobj, &membsz,
			NULL);
	}
}

static void
gen_expr_for(struct gen_context *ctx, const struct expression *expr)
{
	struct qbe_statement lloop, lbody, lvalid, lafter, lend;
	struct qbe_value bloop = mklabel(ctx, &lloop, "loop.%d");
	struct qbe_value bbody = mklabel(ctx, &lbody, "body.%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, "valid.%d");
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct qbe_value bafter = mklabel(ctx, &lafter, "after.%d");

	struct gen_value gcur_object, ginitializer, gptr;
	struct qbe_value qcur_object, qinitializer, qptr, qcur_idx, qlength;

	enum for_kind kind = expr->_for.kind;

	if (kind == FOR_ACCUMULATOR && expr->_for.bindings != NULL) {
		gen_expr_binding(ctx, expr->_for.bindings);
	}

	if (kind == FOR_EACH_VALUE || kind == FOR_EACH_POINTER) {
		ginitializer = gen_autoderef_expr(ctx,
				expr->_for.bindings->binding.initializer);
		qinitializer = mklval(ctx, &ginitializer);

		const struct type *initializer_type = type_dealias(NULL,
			ginitializer.type);

		const struct type *var_type = initializer_type->array.members;

		if (kind == FOR_EACH_POINTER) {
			var_type = type_dealias(NULL,
				expr->_for.bindings->binding.object->type);
		}

		gcur_object = mkgtemp(ctx, var_type, "cur_object.%d");
		qcur_object = mklval(ctx, &gcur_object);

		struct qbe_value qcur_object_sz = constl(var_type->size);
		enum qbe_instr alloc = alloc_for_align(var_type->align);
		pushprei(ctx->current, &qcur_object, alloc, &qcur_object_sz, NULL);

		if (initializer_type->storage == STORAGE_ARRAY) {
			gptr = mkgtemp(ctx, var_type, ".%d");
			qptr = mklval(ctx, &gptr);

			pushi(ctx->current, &qptr, Q_COPY, &qinitializer, NULL);
			qlength = constl(initializer_type->array.length);
		} else {
			assert(initializer_type->storage == STORAGE_SLICE);
			qlength = mkqtmp(ctx, ctx->arch.ptr, "len.%d");

			struct gen_slice slice = gen_slice_ptrs(ctx,
				ginitializer);
			load_slice_data(ctx, &slice, &qptr, &qlength, NULL);

			gptr = (struct gen_value){
				.kind = GV_TEMP,
				.type = var_type,
				.name = qptr.name
			};
		}

		struct qbe_value qzero = constl(0);
		qcur_idx = mkqtmp(ctx, ctx->arch.sz, "cur_idx.%d");
		pushi(ctx->current, &qcur_idx, Q_COPY, &qzero, NULL);
	}

	push_scope(ctx, expr->_for.scope);
	ctx->scope->after = &bafter;
	ctx->scope->end = &bend;

	push(&ctx->current->body, &lloop);

	switch (kind) {
	case FOR_EACH_VALUE:
	case FOR_EACH_POINTER: {
		struct qbe_value qvalid = mkqtmp(ctx, &qbe_word, "valid.%d");

		pushi(ctx->current, &qvalid, Q_CULTL, &qcur_idx, &qlength, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qvalid, &bvalid, &bend, NULL);
		push(&ctx->current->body, &lvalid);

		if (expr->_for.bindings->binding.unpack == NULL) {
			struct expression_binding *binding =
				&expr->_for.bindings->binding;
			if (type_dealias(NULL, binding->object->type)->size != 0) {
				struct gen_binding *gb =
					xcalloc(1, sizeof(struct gen_binding));
				gb->object = binding->object;
				gb->value = gcur_object;
				gb->next = ctx->bindings;
				ctx->bindings = gb;
			}
		}

		if (kind == FOR_EACH_VALUE) {
			gen_store(ctx, gcur_object, gen_load(ctx, gptr));

			struct binding_unpack *unpack =
				expr->_for.bindings->binding.unpack;
			if (unpack != NULL) {
				for (struct binding_unpack *cur_unpack = unpack;
						cur_unpack;
						cur_unpack = cur_unpack->next) {
					if (cur_unpack->object->type->size == 0) {
						continue;
					}
					struct gen_binding *gb = xcalloc(1,
						sizeof(struct gen_binding));

					gb->value = mkgtemp(ctx,
						cur_unpack->object->type,
						"unpack.%d");
					gb->object = cur_unpack->object;
					gb->next = ctx->bindings;
					ctx->bindings = gb;

					struct qbe_value qoff =
						constl(cur_unpack->offset);
					struct qbe_value qitem = mklval(ctx,
						&gb->value);
					pushi(ctx->current, &qitem, Q_ADD,
						&qcur_object, &qoff, NULL);
				}
			}
		} else { // FOR_EACH_POINTER
			enum qbe_instr store = store_for_type(ctx,
				gcur_object.type);
			pushi(ctx->current, NULL, store, &qptr,
				&qcur_object, NULL);
		}

		struct qbe_value qone = constl(1);
		pushi(ctx->current, &qcur_idx, Q_ADD, &qcur_idx, &qone, NULL);
		break;
	}
	case FOR_EACH_ITERATOR:
		ginitializer = gen_expr(ctx,
			expr->_for.bindings->binding.initializer);
		qinitializer = mklval(ctx, &ginitializer);

		const struct type *initializer_type = type_dealias(NULL,
			ginitializer.type);

		struct qbe_value qtag = mkqtmp(ctx, &qbe_word, "tag.%d");
		gen_load_tag(ctx, &qtag, &qinitializer, initializer_type);

		const struct type *done_type = NULL;
		for (const struct type_tagged_union *tu = &initializer_type->tagged;
				tu; tu = tu->next) {
			if (type_dealias(NULL, tu->type)->storage == STORAGE_DONE) {
				done_type = tu->type;
				break;
			}
		}

		struct qbe_value qdone_tag = constw(done_type->id);
		struct qbe_value qisdone = mkqtmp(ctx, &qbe_word, ".%d");

		pushi(ctx->current, &qisdone, Q_CEQW, &qtag, &qdone_tag, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qisdone, &bend, &bvalid, NULL);
		push(&ctx->current->body, &lvalid);

		struct binding_unpack *unpack = expr->_for.bindings->binding.unpack;
		const struct type *var_type;

		if (unpack != NULL) {
			const struct type_tagged_union *tagged = &initializer_type->tagged;
			if  (tagged->type->storage == STORAGE_TUPLE) {
				var_type = tagged->type;
			} else {
				var_type = tagged->next->type;
				assert(var_type->storage == STORAGE_TUPLE);
			}
		} else {
			var_type = expr->_for.bindings->binding.object->type;
		}

		struct qbe_value qptr = qinitializer;
		if (var_type->storage != STORAGE_TAGGED) {
			qptr = mkqtmp(ctx, ctx->arch.ptr, "cur_val.%d");
			struct qbe_value qoffset = nested_tagged_offset(
				ginitializer.type, var_type);
			pushi(ctx->current, &qptr, Q_ADD, &qinitializer, &qoffset, NULL);
		}

		if (unpack != NULL) {
			for (struct binding_unpack *cur_unpack = unpack;
					cur_unpack;
					cur_unpack = cur_unpack->next) {
				if (cur_unpack->object->type->size == 0) {
					continue;
				}
				struct gen_binding *gb =
					xcalloc(1, sizeof(struct gen_binding));

				gb->value = mkgtemp(ctx, cur_unpack->object->type,
					"unpack.%d");
				gb->object = cur_unpack->object;
				gb->next = ctx->bindings;
				ctx->bindings = gb;

				struct qbe_value qoff =	constl(cur_unpack->offset);
				struct qbe_value qitem = mklval(ctx, &gb->value);
				pushi(ctx->current, &qitem, Q_ADD,
					&qptr, &qoff, NULL);
			}
		} else {
			struct expression_binding *binding =
				&expr->_for.bindings->binding;

			if (binding->object->type->size != 0) {
				struct gen_binding *gb =
					xcalloc(1, sizeof(struct gen_binding));

				gb->object = binding->object;
				gb->value = (struct gen_value) {
					.kind = GV_TEMP,
					.type = binding->object->type,
					.name = qptr.name,
				};
				gb->next = ctx->bindings;
				ctx->bindings = gb;
			}
		}
		break;

	case FOR_ACCUMULATOR: {
		struct gen_value cond = gen_expr(ctx, expr->_for.cond);
		struct qbe_value qcond = mkqval(ctx, &cond);

		pushi(ctx->current, NULL, Q_JNZ, &qcond, &bbody, &bend, NULL);
	}}

	push(&ctx->current->body, &lbody);
	gen_expr(ctx, expr->_for.body);

	push(&ctx->current->body, &lafter);
	if (expr->_for.afterthought) {
		gen_expr(ctx, expr->_for.afterthought);
	}
	if (kind == FOR_EACH_VALUE || kind == FOR_EACH_POINTER) {
		struct qbe_value qmember_sz = constl(
			type_dealias(NULL, ginitializer.type)->array.members->size);
		pushi(ctx->current, &qptr, Q_ADD, &qptr, &qmember_sz, NULL);
	}

	pop_scope(ctx);

	pushi(ctx->current, NULL, Q_JMP, &bloop, NULL);

	push(&ctx->current->body, &lend);
}

static void
gen_expr_free(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *type = type_dealias(NULL, expr->free.expr->result);
	if (type->storage == STORAGE_NULL) {
		return;
	}
	struct gen_value val = gen_expr(ctx, expr->free.expr);
	struct qbe_value qval = mkqval(ctx, &val);
	if (type->storage == STORAGE_SLICE || type->storage == STORAGE_STRING) {
		struct qbe_value lval = mklval(ctx, &val);
		qval = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &qval, Q_LOADL, &lval, NULL);
	}
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.free, &qval, NULL);
}

static struct gen_value
gen_expr_if_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement ltrue, lfalse, lend;
	struct qbe_value btrue = mklabel(ctx, &ltrue, "true.%d");
	struct qbe_value bfalse = mklabel(ctx, &lfalse, "false.%d");
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct gen_value cond = gen_expr(ctx, expr->_if.cond);
	struct qbe_value qcond = mkqval(ctx, &cond);
	qcond = extend(ctx, qcond, &builtin_type_bool);
	pushi(ctx->current, NULL, Q_JNZ, &qcond, &btrue, &bfalse, NULL);

	push(&ctx->current->body, &ltrue);
	gen_expr_branch(ctx, expr->_if.true_branch, gvout, out);
	if (expr->_if.true_branch->result->storage != STORAGE_NEVER) {
		pushi(ctx->current, NULL, Q_JMP, &bend, NULL);
	}

	push(&ctx->current->body, &lfalse);
	gen_expr_branch(ctx, expr->_if.false_branch, gvout, out);
	push(&ctx->current->body, &lend);
	return gvout;
}

static struct gen_value
gen_expr_append_insert_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct qbe_statement lvalid, loom, lend;
	struct qbe_value bvalid = mklabel(ctx, &lvalid, "valid.%d");
	struct qbe_value bend = mklabel(ctx, &lend, "end.%d");
	struct qbe_value boom = mklabel(ctx, &loom, "oom.%d");

	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	struct qbe_value qout = mkqval(ctx, out ? out : &gvout);

	if (!out) {
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		struct qbe_value sz = constl(expr->result->size);
		pushprei(ctx->current, &qout, alloc, &sz, NULL);
	}

	struct gen_value slice;
	if (expr->type == EXPR_APPEND) {
		slice = gen_autoderef_expr(ctx, expr->append.object);
	} else {
		const struct expression *objexpr = expr->append.object;
		assert(objexpr->type == EXPR_ACCESS
			&& objexpr->access.type == ACCESS_INDEX);
		slice = gen_autoderef_expr(ctx, objexpr->access.array);
	}
	struct qbe_value prevlen, cap;
	struct gen_slice sl = gen_slice_ptrs(ctx, slice);
	load_slice_data(ctx, &sl, NULL, &prevlen, expr->append.is_static ? &cap : NULL);

	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	struct qbe_value qindex;
	if (expr->type == EXPR_APPEND) {
		qindex = prevlen;
	} else {
		struct gen_value index = gen_expr(ctx,
			expr->append.object->access.index);
		qindex = mkqval(ctx, &index);
		gen_fixed_assert(ctx, expr->loc, ABORT_OOB, Q_CULEL, &qindex, &prevlen);
	}
	struct qbe_value qvoid_id = constw(builtin_type_void.id);

	struct qbe_value appendlen;
	const struct type *valtype = type_dealias(NULL, expr->append.value->result);
	if (expr->append.length != NULL) {
		struct gen_value length = gen_expr(ctx, expr->append.length);
		if (expr->append.length->result->storage == STORAGE_NEVER) {
			return gvout;
		}
		appendlen = mkqval(ctx, &length);
		assert(valtype->storage == STORAGE_ARRAY && valtype->array.expandable);
	} else if (!expr->append.is_multi) {
		appendlen = constl(1);
	}

	struct gen_value value;
	struct qbe_value qvalue;
	if (!expr->append.is_multi || valtype->storage != STORAGE_ARRAY) {
		// We use gen_expr_at for the array case to avoid a copy
		value = gen_expr(ctx, expr->append.value);
		if (expr->append.value->result->storage == STORAGE_NEVER) {
			return gvout;
		}
		qvalue = mkqval(ctx, &value);
	}

	if (expr->append.is_multi) {
		if (valtype->storage == STORAGE_ARRAY) {
			assert(valtype->array.length != SIZE_UNDEFINED);
			appendlen = constl(valtype->array.length);
		} else {
			appendlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
			struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
			struct qbe_value offs = constl(ctx->arch.ptr->size);
			pushi(ctx->current, &ptr, Q_ADD, &qvalue, &offs, NULL);
			pushi(ctx->current, &appendlen, load, &ptr, NULL);
		}
	}

	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &newlen, Q_ADD, &prevlen, &appendlen, NULL);
	store_slice_data(ctx, &sl, NULL, &newlen, NULL);

	const struct type *mtype = type_dealias(NULL, slice.type)->array.members;
	struct qbe_value membsz = constl(mtype->size);
	struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
	if (expr->append.is_static) {
		pushi(ctx->current, &cmpres, Q_CULEL, &newlen, &cap, NULL);
	} else {
		struct qbe_value lval = mklval(ctx, &slice);
		pushi(ctx->current, &cmpres, Q_CALL, &ctx->rt.ensure, &lval, &membsz, NULL);
	}

	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bvalid, &boom, NULL);
	push(&ctx->current->body, &lvalid);

	struct gen_value item = mkgtemp(ctx, mtype, ".%d");
	struct qbe_value ptr = mklval(ctx, &item);
	struct qbe_value base = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &base, load, &sl.base, NULL);
	pushi(ctx->current, &ptr, Q_MUL, &qindex, &membsz, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &base, &ptr, NULL);

	struct qbe_value nbyte = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &nbyte, Q_MUL, &appendlen, &membsz, NULL);

	if (expr->type == EXPR_INSERT) {
		struct qbe_value dest = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		struct qbe_value ncopy = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &ncopy, Q_SUB, &prevlen, &qindex, NULL);
		pushi(ctx->current, &ncopy, Q_MUL, &ncopy, &membsz, NULL);
		pushi(ctx->current, &dest, Q_ADD, &ptr, &nbyte, NULL);

		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove,
			&dest, &ptr, &ncopy, NULL);
	}

	if (expr->append.is_multi && valtype->storage == STORAGE_ARRAY) {
		item.type = valtype;
		gen_expr_at(ctx, expr->append.value, item);
	} else if (expr->append.is_multi && valtype->storage == STORAGE_SLICE) {
		struct qbe_value qsrc = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &qsrc, load, &qvalue, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &ptr, &qsrc, &nbyte, NULL);
	} else if (expr->append.length != NULL) {
		// XXX: This could be made more efficient for some cases if
		// check could determine the length at compile time and lower it
		// to a fixed-length array type
		assert(valtype->storage == STORAGE_ARRAY);
		item.type = valtype;
		gen_expr_at(ctx, expr->append.value, item);

		assert(valtype->array.length != SIZE_UNDEFINED);
		struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, "next.%d");
		struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, "last.%d");
		struct qbe_value arlen = constl(valtype->array.length * mtype->size);
		pushi(ctx->current, &next, Q_ADD, &ptr, &arlen, NULL);
		arlen = constl((valtype->array.length - 1) * mtype->size);
		pushi(ctx->current, &last, Q_ADD, &ptr, &arlen, NULL);

		struct qbe_value remain = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		struct qbe_value one = constl(1);
		pushi(ctx->current, &remain, Q_SUB, &appendlen, &one, NULL);
		pushi(ctx->current, &remain, Q_MUL, &remain, &membsz, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &remain, NULL);
	} else {
		gen_store(ctx, item, value);
	}

	gen_store_tag(ctx, &qout, expr->result, &qvoid_id);

	pushi(ctx->current, NULL, Q_JMP, &bend, NULL);
	push(&ctx->current->body, &loom);

	struct qbe_value qnomem_id = constw(builtin_type_nomem.id);
	gen_store_tag(ctx, &qout, expr->result, &qnomem_id);

	push(&ctx->current->body, &lend);

	return gvout;
}

enum match_compat {
	// The case type is a member of the match object type and can be used
	// directly from the match object's tagged union storage area.
	COMPAT_SUBTYPE,
	// The case type is a tagged union which is a subset of the object type.
	COMPAT_SUBSET,
};

static struct qbe_value
nested_tagged_offset(const struct type *tu, const struct type *target)
{
	// This function calculates the offset of a member in a nested tagged union
	//
	// type foo = (int | void);
	// type bar = (size | foo);
	//
	// The offset of the "foo" field from the start of "bar" is 4, and the
	// offset of int inside "foo" is 4, so the offset of int from the start
	// of "bar" is 8. The size is at offset 8.
	const struct type *tu_memb;
	uint64_t offset = 0;

	do {
		offset += builtin_type_u32.align;
		tu_memb = tagged_select_subtype(NULL, tu, target, false);
		if (!tu_memb) {
			break;
		}
		if (tu_memb->align != 0 && offset % tu_memb->align != 0) {
			offset += tu_memb->align - offset % tu_memb->align;
		}
		tu = tu_memb;
	} while (tu_memb->id != target->id && type_dealias(NULL, tu_memb)->id != target->id);
	return constl(offset);
}

static struct gen_value
gen_nested_match_tests(struct gen_context *ctx, struct gen_value object,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type, const struct type *subtype)
{
	// This function handles the case where we're matching against a type
	// which is a member of the tagged union, or an inner tagged union.
	//
	// type foo = (int | void);
	// type bar = (size | foo);
	//
	// let x: bar = 10i;
	// match (x) {
	// case let z: size => ...
	// case let i: int => ...
	// case	void => ...
	// };
	//
	// In the first case, we can simply test the object's tag. In the second
	// case, we have to test if the selected tag is 'foo', then check the
	// tag of the foo object for int.
	struct qbe_value *subtag = &tag;
	struct qbe_value subval = mkcopy(ctx, &object, "subval.%d");
	struct gen_value match = mkgtemp(ctx, &builtin_type_bool, ".%d");
	struct qbe_value qmatch = mkqval(ctx, &match);
	struct qbe_value temp = mkqtmp(ctx, &qbe_word, ".%d");
	const struct type *test = subtype;
	do {
		struct qbe_statement lsubtype;
		struct qbe_value bsubtype = mklabel(ctx, &lsubtype, "subtype.%d");

		if (type_dealias(NULL, type)->storage != STORAGE_TAGGED) {
			break;
		}
		test = tagged_select_subtype(NULL, type, subtype, false);
		if (!test) {
			break;
		}

		struct qbe_value id = constw(test->id);
		pushi(ctx->current, &qmatch, Q_CEQW, subtag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qmatch, &bsubtype, &bnext, NULL);
		push(&ctx->current->body, &lsubtype);

		// In the case of a tagged union which is a subset of the
		// object, where we're testing for a type within that subset,
		// move the pointer to this tagged union and continue looking
		// for the relevant type ID there.
		if (test->id != subtype->id
				&& type_dealias(NULL, test)->id != subtype->id
				&& type_dealias(NULL, test)->storage == STORAGE_TAGGED) {
			struct qbe_value offs =
				compute_tagged_memb_offset(test);
			pushi(ctx->current, &subval, Q_ADD, &subval, &offs, NULL);
			gen_load_tag(ctx, &temp, &subval, test);
			subtag = &temp;
		}

		type = test;
	} while (test->id != subtype->id && type_dealias(NULL, test)->id != subtype->id);

	pushi(ctx->current, NULL, Q_JMP, &bmatch, NULL);
	return match;
}

static struct gen_value
gen_subset_match_tests(struct gen_context *ctx,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type)
{
	// In this case, we're testing a case which is itself a tagged union,
	// and is a subset of the match object.
	//
	// type foo = (size | int | void);
	//
	// let x: foo = 10i;
	// match (x) {
	// case let n: (size | int) => ...
	// case void => ...
	// };
	//
	// In this situation, we test the match object's tag against each type
	// ID of the case type.
	struct gen_value match = mkgtemp(ctx, &builtin_type_bool, ".%d");
	for (const struct type_tagged_union *tu = &type->tagged; tu; tu = tu->next) {
		struct qbe_statement lnexttag;
		struct qbe_value bnexttag = mklabel(ctx, &lnexttag, ".%d");
		struct qbe_value id = constl(tu->type->id);
		struct qbe_value qmatch = mkqval(ctx, &match);
		pushi(ctx->current, &qmatch, Q_CEQW, &tag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qmatch, &bmatch, &bnexttag, NULL);
		push(&ctx->current->body, &lnexttag);
	}
	pushi(ctx->current, NULL, Q_JMP, &bnext, NULL);
	return match;
}

static struct gen_value
gen_expr_match_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value object = gen_expr(ctx, expr->match.value);
	struct qbe_value qobject = mkqval(ctx, &object);
	const struct type *objtype = type_dealias(NULL,
		expr->match.value->result);

	bool is_tagged = objtype->storage == STORAGE_TAGGED;
	bool is_nullable_ptr = false, is_tagged_ptr = false;
	// If objtype is a pointer, ref_type is the dealiased referent type.
	const struct type *ref_type = objtype;
	if (objtype->storage == STORAGE_POINTER) {
		is_nullable_ptr = objtype->pointer.nullable;
		ref_type = type_dealias(NULL, objtype->pointer.referent);
		is_tagged_ptr = ref_type->storage == STORAGE_TAGGED;
	}
	assert(is_tagged || is_nullable_ptr || is_tagged_ptr);

	// Pass over the list once to find the default case and null case, if
	// they exist. For matching on nullable pointers to tagged unions, we
	// need to check for null before we dereference the pointer to check the
	// tag.
	const struct match_case *default_case = NULL;
	const struct match_case *null_case = NULL;
	for (const struct match_case *c = expr->match.cases; c; c = c->next) {
		if (!c->type) {
			default_case = c;
		} else if (c->type->storage == STORAGE_NULL) {
			null_case = c;
		}
	}
	if (is_nullable_ptr) {
		assert(default_case || null_case);
	}

	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}
	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");

	struct qbe_statement ldefault;
	struct qbe_value bdefault = mklabel(ctx, &ldefault, "default.%d");
	struct qbe_value zero = constl(0);
	if (is_nullable_ptr) {
		struct qbe_statement lmatch, lnext;
		struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnext = mklabel(ctx, &lnext, "next.%d");
		// If there is no null case, go to the default case on null.
		struct qbe_value *where = null_case ? &bmatch : &bdefault;
		pushi(ctx->current, &cmpres, Q_CEQL, &qobject, &zero, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &cmpres, where, &bnext, NULL);
		push(&ctx->current->body, &lmatch);
		if (null_case) {
			gen_expr_branch(ctx, null_case->value, gvout, out);
			if (null_case->value->result->storage != STORAGE_NEVER) {
				pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
			}
		}
		push(&ctx->current->body, &lnext);
	}

	struct qbe_value tag = mkqtmp(ctx, &qbe_word, "tag.%d");
	if (is_tagged || is_tagged_ptr) {
		gen_load_tag(ctx, &tag, &qobject, ref_type);
	}

	for (const struct match_case *c = expr->match.cases; c; c = c->next) {
		if (c == null_case || c == default_case) {
			continue;
		}

		struct qbe_statement lmatch, lnext;
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnext = mklabel(ctx, &lnext, "next.%d");
		if (is_nullable_ptr && !is_tagged_ptr) {
			struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
			pushi(ctx->current, &cmpres, Q_CNEL, &qobject, &zero,
				NULL);
			pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bmatch,
				&bnext, NULL);
			push(&ctx->current->body, &lmatch);

			if (c->object) {
				struct gen_binding *gb = xcalloc(1,
					sizeof(struct gen_binding));
				gb->value = mkgtemp(ctx, c->type, "binding.%d");
				gb->object = c->object;
				gb->next = ctx->bindings;
				ctx->bindings = gb;

				enum qbe_instr store = store_for_type(ctx,
					c->type);
				enum qbe_instr alloc = alloc_for_align(
					c->type->align);
				struct qbe_value qv = mkqval(ctx, &gb->value);
				struct qbe_value sz = constl(c->type->size);
				pushprei(ctx->current, &qv, alloc, &sz, NULL);
				pushi(ctx->current, NULL, store, &qobject, &qv,
					NULL);
			}
		} else {
			const struct type *case_tag_type = c->type;
			if (is_tagged_ptr && c->type->size > 0) {
				case_tag_type = c->type->pointer.referent;
			};
			const struct type *subtype = tagged_select_subtype(NULL,
				ref_type, case_tag_type, false);
			enum match_compat compat = COMPAT_SUBTYPE;
			if (subtype) {
				gen_nested_match_tests(ctx, object,
					bmatch, bnext, tag, ref_type, case_tag_type);
			} else {
				assert(type_dealias(NULL, case_tag_type)
					->storage == STORAGE_TAGGED);
				assert(tagged_subset_compat(NULL, ref_type,
					case_tag_type));
				compat = COMPAT_SUBSET;
				const struct type *casetype = type_dealias(NULL,
					case_tag_type);
				gen_subset_match_tests(ctx, bmatch, bnext, tag,
					casetype);
			}

			push(&ctx->current->body, &lmatch);

			if (c->object && c->type->size > 0) {
				struct gen_binding *gb = xcalloc(1,
					sizeof(struct gen_binding));
				gb->value = mkgtemp(ctx, c->type, "binding.%d");
				gb->object = c->object;
				gb->next = ctx->bindings;
				ctx->bindings = gb;

				struct qbe_value qv = mklval(ctx, &gb->value);
				enum qbe_instr alloc = alloc_for_align(
					c->type->align);
				struct qbe_value sz = constl(c->type->size);
				pushprei(ctx->current, &qv, alloc, &sz, NULL);

				struct gen_value src = mkgtemp(ctx, c->type, ".%d");
				struct qbe_value ptr = mklval(ctx, &src);
				struct gen_value load;
				struct qbe_value offset;
				switch (compat) {
				case COMPAT_SUBTYPE:
					offset = nested_tagged_offset(ref_type,
						case_tag_type);
					pushi(ctx->current, &ptr, Q_ADD,
						&qobject, &offset, NULL);
					break;
				case COMPAT_SUBSET:
					pushi(ctx->current, &ptr, Q_COPY,
						&qobject, NULL);
					break;
				}
				if (is_tagged) {
					load = gen_load(ctx, src);
					gen_store(ctx, gb->value, load);
				} else {
					gen_store(ctx, gb->value, src);
				}
			}
		}
		gen_expr_branch(ctx, c->value, gvout, out);
		if (c->value->result->storage != STORAGE_NEVER) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
		push(&ctx->current->body, &lnext);
	}

	if (default_case) {
		push(&ctx->current->body, &ldefault);
		gen_expr_branch(ctx, default_case->value, gvout, out);
		if (default_case->value->result->storage != STORAGE_NEVER) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
	} else {
		// We can remove this abort once we have exhaustivity checking.
		struct qbe_statement labort;
		mklabel(ctx, &labort, ".%d");
		push(&ctx->current->body, &labort);
		gen_fixed_abort(ctx, expr->loc, ABORT_UNREACHABLE);
	}
	push(&ctx->current->body, &lout);
	return gvout;
}

static struct gen_value
gen_expr_len(struct gen_context *ctx, const struct expression *expr)
{
	const struct expression *value = expr->len.value;
	const struct type *type = type_dereference(NULL, value->result, false);
	assert(type != NULL);
	type = type_dealias(NULL, type);
	assert(type->storage == STORAGE_SLICE || type->storage == STORAGE_STRING);
	struct gen_value gv = gen_autoderef_expr(ctx, value);
	struct qbe_value len;
	struct gen_slice sl = gen_slice_ptrs(ctx, gv);
	load_slice_data(ctx, &sl, NULL, &len, NULL);
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = &builtin_type_size,
		.name = len.name
	};
}

static void
gen_expr_return(struct gen_context *ctx, const struct expression *expr)
{
	if (expr->_return.value->result->storage == STORAGE_NEVER) {
		gen_expr(ctx, expr->_return.value);
		return;
	}
	struct gen_value ret = gen_expr(ctx, expr->_return.value);
	for (struct gen_scope *scope = ctx->scope; scope; scope = scope->parent) {
		gen_defers(ctx, scope);
	}
	if (ret.type->size == 0) {
		pushi(ctx->current, NULL, Q_RET, NULL);
	} else {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	}
}

static void
gen_expr_struct_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	// TODO: Merge me into literal expressions
	struct qbe_value base = mkqval(ctx, &out);

	if (expr->_struct.autofill) {
		struct qbe_value size =
			constl(expr->result->size), zero = constl(0);
		struct qbe_value base = mklval(ctx, &out);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memset,
			&base, &zero, &size, NULL);
	}

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct expr_struct_field *field = expr->_struct.fields;
			field; field = field->next) {
		if (!field->value) {
			assert(expr->_struct.autofill);
			continue;
		}

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static struct gen_value
gen_expr_struct(struct gen_context *ctx, const struct expression *expr)
{
	if (expr->result->size != 0) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mklval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_struct_at(ctx, expr, out);
		return out;
	}

	for (const struct expr_struct_field *field = expr->_struct.fields;
			field; field = field->next) {
		gen_expr(ctx, field->value);
	}
	return gv_void;
}

static struct gen_value
gen_expr_switch_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
		if (expr->result->storage != STORAGE_NEVER && expr->result->size != 0) {
			// XXX: hack, to make gvout appear initialized to QBE
			struct qbe_value qout = mkqval(ctx, &gvout);
			struct qbe_value zero = constl(0);
			pushi(ctx->current, &qout, Q_COPY, &zero, NULL);
		}
	}

	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");
	struct gen_value value = gen_expr(ctx, expr->_switch.value);

	const struct switch_case *_default = NULL;
	for (const struct switch_case *_case = expr->_switch.cases;
			_case; _case = _case->next) {
		if (!_case->options) {
			_default = _case;
			continue;
		}

		struct qbe_statement lmatch, lnextcase;
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnextcase = mklabel(ctx, &lnextcase, "next.%d");

		for (struct case_option *opt = _case->options;
				opt; opt = opt->next) {
			struct qbe_statement lnextopt;
			struct qbe_value bnextopt = mklabel(ctx, &lnextopt, ".%d");
			struct gen_value test = gen_expr_literal(ctx, opt->value);
			struct gen_value match = gen_expr_binarithm_gv(ctx,
				&builtin_type_bool, expr->_switch.value->result,
				opt->value->result, BIN_LEQUAL, value, test);
			struct qbe_value cond = mkqval(ctx, &match);
			pushi(ctx->current, NULL, Q_JNZ,
				&cond, &bmatch, &bnextopt, NULL);
			push(&ctx->current->body, &lnextopt);
		}

		pushi(ctx->current, NULL, Q_JMP, &bnextcase, NULL);
		push(&ctx->current->body, &lmatch);
		gen_expr_branch(ctx, _case->value, gvout, out);
		if (_case->value->result->storage != STORAGE_NEVER) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
		push(&ctx->current->body, &lnextcase);
	}

	if (_default) {
		gen_expr_branch(ctx, _default->value, gvout, out);
		if (_default->value->result->storage != STORAGE_NEVER) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
	}

	struct qbe_statement labort;
	mklabel(ctx, &labort, ".%d");
	push(&ctx->current->body, &labort);
	gen_fixed_abort(ctx, expr->loc, ABORT_UNREACHABLE);

	push(&ctx->current->body, &lout);
	return gvout;
}

static void
gen_expr_slice_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct gen_value object = gen_autoderef_expr(ctx, expr->slice.object);
	const struct type *srctype = type_dealias(NULL, object.type);

	struct qbe_value qbase, qstart, qnewlen, qnewcap;
	struct qbe_value qlen_, qcap_, *qlen = &qlen_, *qcap = &qcap_;
	if (srctype->storage == STORAGE_ARRAY) {
		qbase = mkcopy(ctx, &object, ".%d");
		if (srctype->array.length == SIZE_UNDEFINED) {
			qcap = qlen = NULL;
		} else {
			*qcap = *qlen = constl(srctype->array.length);
		}
	} else {
		struct gen_slice sl = gen_slice_ptrs(ctx, object);
		load_slice_data(ctx, &sl, &qbase, qlen, qcap);
	}
	gen_subslice_info(ctx, expr, qlen, qcap, &qstart, NULL, &qnewlen, &qnewcap);

	if (srctype->array.members->size != SIZE_UNDEFINED) {
		struct qbe_value data = mkqtmp(ctx, ctx->arch.sz, ".%d");
		struct qbe_value isz = constl(srctype->array.members->size);
		pushi(ctx->current, &data, Q_MUL, &qstart, &isz, NULL);
		pushi(ctx->current, &qbase, Q_ADD, &qbase, &data, NULL);
	}

	struct gen_slice sl = gen_slice_ptrs(ctx, out);
	store_slice_data(ctx, &sl, &qbase, &qnewlen, &qnewcap);
}

static void
gen_expr_tuple_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	// TODO: Merge me into literal expressions
	struct qbe_value base = mkqval(ctx, &out);

	const struct type *type = type_dealias(NULL, expr->result);
	struct gen_value vtemp = mkgtemp(ctx, &builtin_type_void, "value.%d");
	const struct expression_tuple *value = &expr->tuple;
	for (const struct type_tuple *tuple = &type->tuple;
			tuple; tuple = tuple->next) {
		struct qbe_value offs = constl(tuple->offset);
		vtemp.type = value->value->result;
		struct qbe_value ptr = mklval(ctx, &vtemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, value->value, vtemp);
		value = value->next;
	}
}

static struct gen_value
gen_expr_tuple(struct gen_context *ctx, const struct expression *expr)
{
	if (expr->result->size != 0) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mklval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_tuple_at(ctx, expr, out);
		return out;
	}

	for (const struct expression_tuple *value = &expr->tuple;
			value; value = value->next) {
		gen_expr(ctx, value->value);
	}
	return gv_void;
}

static struct gen_value
gen_expr_unarithm(struct gen_context *ctx,
	const struct expression *expr)
{
	struct gen_value val, temp;
	struct qbe_value qval, qtmp;
	const struct expression *operand = expr->unarithm.operand;
	switch (expr->unarithm.op) {
	case UN_ADDRESS:
		if (operand->type == EXPR_ACCESS) {
			val = gen_expr_access_addr(ctx, operand);
			val.type = expr->result;
			return val;
		}
		struct gen_value val = mkgtemp(ctx, operand->result, ".%d");
		struct qbe_value qv = mklval(ctx, &val);
		struct qbe_value sz = constl(val.type->size);
		enum qbe_instr alloc = alloc_for_align(val.type->align);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);
		gen_expr_at(ctx, operand, val);
		val.type = expr->result;
		return val;
	case UN_DEREF:
		val = gen_expr(ctx, operand);
		assert(type_dealias(NULL, val.type)->storage == STORAGE_POINTER);
		val.type = type_dealias(NULL, val.type)->pointer.referent;
		return gen_load(ctx, val);
	case UN_BNOT:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		struct qbe_value ones = constl((uint64_t)-1);
		pushi(ctx->current, &qtmp, Q_XOR, &qval, &ones, NULL);
		return temp;
	case UN_LNOT:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		qval = extend(ctx, qval, operand->result);
		struct qbe_value zerow = constw(0);
		pushi(ctx->current, &qtmp, Q_CEQW, &qval, &zerow, NULL);
		return temp;
	case UN_MINUS:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		pushi(ctx->current, &qtmp, Q_NEG, &qval, NULL);
		return temp;
	}
	abort(); // Invariant
}

static struct gen_value
gen_expr_vaarg(struct gen_context *ctx,
	const struct expression *expr)
{
	// XXX: qbe only supports variadic base types, should check for this
	assert(expr->result->size != SIZE_UNDEFINED);
	if (expr->result->size == 0) {
		return gv_void;
	}
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	struct gen_value ap = gen_expr(ctx, expr->vaarg.ap);
	struct qbe_value qap = mkqval(ctx, &ap);
	pushi(ctx->current, &qresult, Q_VAARG, &qap, NULL);
	return result;
}

static void
gen_expr_vastart_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct qbe_value base = mklval(ctx, &out);
	pushi(ctx->current, NULL, Q_VASTART, &base, NULL);
}

static struct gen_value
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
	if (expr->loc.file && expr->loc.lineno) {
		struct qbe_value qline = constl(expr->loc.lineno);
		struct qbe_value qcol = constl(expr->loc.colno);
		pushi(ctx->current, NULL, Q_DBGLOC, &qline, &qcol, NULL);
	}

	struct gen_value out = gv_void;
	switch ((int)expr->type) {
	case EXPR_ACCESS:
		out = gen_expr_access(ctx, expr);
		break;
	case EXPR_APPEND:
	case EXPR_INSERT:
		out = gen_expr_append_insert_with(ctx, expr, NULL);
		break;
	case EXPR_ALLOC:
		out = gen_expr_alloc_with(ctx, expr, NULL);
		break;
	case EXPR_ASSERT:
		gen_expr_assert(ctx, expr);
		break;
	case EXPR_ASSIGN:
		gen_expr_assign(ctx, expr);
		break;
	case EXPR_BINARITHM:
		out = gen_expr_binarithm(ctx, expr);
		break;
	case EXPR_BINDING:
		gen_expr_binding(ctx, expr);
		break;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_YIELD:
		gen_expr_control(ctx, expr);
		break;
	case EXPR_CALL:
		out = gen_expr_call(ctx, expr);
		break;
	case EXPR_CAST:
		out = gen_expr_cast(ctx, expr);
		break;
	case EXPR_COMPOUND:
		out = gen_expr_compound_with(ctx, expr, NULL);
		break;
	case EXPR_LITERAL:
		out = gen_expr_literal(ctx, expr);
		break;
	case EXPR_DEFER:
		gen_expr_defer(ctx, expr);
		break;
	case EXPR_DELETE:
		gen_expr_delete(ctx, expr);
		break;
	case EXPR_FOR:
		gen_expr_for(ctx, expr);
		break;
	case EXPR_FREE:
		gen_expr_free(ctx, expr);
		break;
	case EXPR_IF:
		out = gen_expr_if_with(ctx, expr, NULL);
		break;
	case EXPR_LEN:
		out = gen_expr_len(ctx, expr);
		break;
	case EXPR_MATCH:
		out = gen_expr_match_with(ctx, expr, NULL);
		break;
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (for now?)
	case EXPR_RETURN:
		gen_expr_return(ctx, expr);
		break;
	case EXPR_SWITCH:
		out = gen_expr_switch_with(ctx, expr, NULL);
		break;
	case EXPR_UNARITHM:
		out = gen_expr_unarithm(ctx, expr);
		break;
	case EXPR_VAARG:
		out = gen_expr_vaarg(ctx, expr);
		break;
	case EXPR_STRUCT:
		out = gen_expr_struct(ctx, expr);
		break;
	case EXPR_TUPLE:
		out = gen_expr_tuple(ctx, expr);
		break;
	case EXPR_SLICE:
	case EXPR_VASTART:
		// Prefers -at style
		out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_at(ctx, expr, out);
		break;
	case EXPR_DEFINE:
	case EXPR_VAEND:
		break;
	}

	if (expr->result->storage == STORAGE_NEVER) {
		// XXX: This is a bit hacky, to appease qbe
		struct qbe_statement dummyl;
		mklabel(ctx, &dummyl, ".%d");
		push(&ctx->current->body, &dummyl);
		return gv_void;
	}
	return out;
}

static void
gen_expr_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	assert(out.kind != GV_CONST);

	switch (expr->type) {
	case EXPR_ALLOC:
		gen_expr_alloc_with(ctx, expr, &out);
		return;
	case EXPR_APPEND:
	case EXPR_INSERT:
		out = gen_expr_append_insert_with(ctx, expr, &out);
		return;
	case EXPR_CAST:
		gen_expr_cast_at(ctx, expr, out);
		return;
	case EXPR_COMPOUND:
		gen_expr_compound_with(ctx, expr, &out);
		return;
	case EXPR_LITERAL:
		gen_expr_literal_at(ctx, expr, out);
		return;
	case EXPR_IF:
		gen_expr_if_with(ctx, expr, &out);
		return;
	case EXPR_MATCH:
		gen_expr_match_with(ctx, expr, &out);
		return;
	case EXPR_SLICE:
		gen_expr_slice_at(ctx, expr, out);
		return;
	case EXPR_STRUCT:
		gen_expr_struct_at(ctx, expr, out);
		return;
	case EXPR_SWITCH:
		gen_expr_switch_with(ctx, expr, &out);
		return;
	case EXPR_TUPLE:
		gen_expr_tuple_at(ctx, expr, out);
		return;
	case EXPR_VASTART:
		gen_expr_vastart_at(ctx, expr, out);
		return;
	default:
		break; // Prefers non-at style
	}

	if (expr->result->storage == STORAGE_NEVER) {
		gen_expr(ctx, expr);
		return;
	}
	gen_store(ctx, out, gen_expr(ctx, expr));
}

static void
gen_expr_branch(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value merged,
	struct gen_value *out)
{
	// Branching expressions written in the _with style may need to
	// consolidate each branch's result into a single temporary to return to
	// the caller. This function facilitates that.
	if (expr->result->storage == STORAGE_NEVER || expr->result->size == 0) {
		gen_expr(ctx, expr);
	} else if (out) {
		gen_expr_at(ctx, expr, *out);
	} else {
		assert(expr->result == merged.type);
		struct gen_value result = gen_expr(ctx, expr);
		struct qbe_value qresult = mkqval(ctx, &result);
		struct qbe_value qmerged = mkqval(ctx, &merged);
		pushi(ctx->current, &qmerged, Q_COPY, &qresult, NULL);
	}
}

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	if (func->body == NULL) {
		return; // Prototype
	}

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	ctx->current = &qdef->func;

	qdef->name = decl->symbol ? decl->symbol : ident_to_sym(ctx->itbl, decl->ident);
	qdef->file = decl->file;

	struct qbe_statement start_label = {0};
	mklabel(ctx, &start_label, "start.%d");
	push(&qdef->func.prelude, &start_label);

	if (fntype->func.result->size != 0
			&& fntype->func.result->storage != STORAGE_NEVER) {
		qdef->func.returns = qtype_lookup(
			ctx, fntype->func.result, true);
	} else {
		qdef->func.returns = &qbe_void;
	}
	if (fntype->func.variadism == VARIADISM_C) {
		qdef->func.variadic = true;
	}

	struct qbe_func_param *param, **next = &qdef->func.params;
	for (struct scope_object *obj = decl->func.scope->objects;
			obj; obj = obj->lnext) {
		const struct type *type = obj->type;
		if (type->size == 0) {
			continue;
		}
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident->ns); // Invariant
		param->name = xstrdup(obj->ident->name);
		param->type = qtype_lookup(ctx, type, false);

		struct gen_binding *gb =
			xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->object = obj;
		if (type_is_aggregate(type)) {
			// No need to copy to stack
			gb->value.name = xstrdup(param->name);
		} else {
			gb->value.name = gen_name(&ctx->id, "param.%d");

			struct qbe_value qv = mklval(ctx, &gb->value);
			struct qbe_value sz = constl(type->size);
			enum qbe_instr alloc = alloc_for_align(type->align);
			pushprei(ctx->current, &qv, alloc, &sz, NULL);
			struct gen_value src = {
				.kind = GV_TEMP,
				.type = type,
				.name = param->name,
			};
			gen_store(ctx, gb->value, src);
		}

		gb->next = ctx->bindings;
		ctx->bindings = gb;
		next = &param->next;
	}

	struct qbe_statement lbody;
	mklabel(ctx, &lbody, "body.%d");
	push(&ctx->current->body, &lbody);
	struct gen_value ret = gen_expr(ctx, decl->func.body);

	if (fntype->func.result->storage == STORAGE_NEVER) {
		pushi(ctx->current, NULL, Q_HLT, NULL);
	} else if (decl->func.body->result->storage == STORAGE_NEVER) {
		// XXX: This is a bit hacky, to appease qbe
		size_t ln = ctx->current->body.ln;
		struct qbe_statement *last = &ctx->current->body.stmts[ln - 1];
		if (last->type != Q_INSTR || last->instr != Q_RET) {
			pushi(ctx->current, NULL, Q_HLT, NULL);
		}
	} else if (fntype->func.result->size != 0) {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	} else {
		pushi(ctx->current, NULL, Q_RET, NULL);
	}

	qbe_append_def(ctx->out, qdef);

	if (func->flags & FN_INIT) {
		struct qbe_def *init = xcalloc(1, sizeof *init);
		init->kind = Q_DATA;
		init->exported = false;
		init->data.align = 8;
		init->data.section = ".init_array";
		init->data.secflags = NULL;

		size_t n = snprintf(NULL, 0, ".init.%s", qdef->name);
		char *s = xcalloc(n + 1, 1);
		snprintf(s, n + 1, ".init.%s", qdef->name);
		init->name = intern_owned(ctx->itbl, s);

		struct qbe_data_item dataitem = {
			.type = QD_VALUE,
			.value = {
				.kind = QV_GLOBAL,
				.type = ctx->arch.ptr,
				.name = xstrdup(qdef->name),
			},
			.next = NULL,
		};
		init->data.items = dataitem;

		qbe_append_def(ctx->out, init);
	}

	if (func->flags & FN_FINI) {
		struct qbe_def *fini = xcalloc(1, sizeof *fini);
		fini->kind = Q_DATA;
		fini->exported = false;
		fini->data.align = 8;
		fini->data.section = ".fini_array";
		fini->data.secflags = NULL;

		size_t n = snprintf(NULL, 0, ".fini.%s", qdef->name);
		char *s = xcalloc(n + 1, 1);
		snprintf(s, n + 1, ".fini.%s", qdef->name);
		fini->name = intern_owned(ctx->itbl, s);

		struct qbe_data_item dataitem = {
			.type = QD_VALUE,
			.value = {
				.kind = QV_GLOBAL,
				.type = ctx->arch.ptr,
				.name = xstrdup(qdef->name),
			},
			.next = NULL,
		};
		fini->data.items = dataitem;

		qbe_append_def(ctx->out, fini);
	}

	if (func->flags & FN_TEST) {
		struct qbe_def *test = xcalloc(1, sizeof *test);
		test->kind = Q_DATA;
		test->exported = false;
		test->data.align = 8;
		test->data.section = ".test_array";
		test->data.secflags = "aw";

		size_t n = snprintf(NULL, 0, ".test.%s", qdef->name);
		char *s = xcalloc(n + 1, 1);
		snprintf(s, n + 1, ".test.%s", qdef->name);
		test->name = intern_owned(ctx->itbl, s);

		char *ident = ident_unparse(decl->ident);
		struct qbe_data_item *dataitem = &test->data.items;
		struct expression expr;
		mkstrliteral(&expr, "%s", ident);
		free(ident);
		dataitem = gen_data_item(ctx, &expr, dataitem);

		struct qbe_data_item *next = xcalloc(1, sizeof *next);
		next->type = QD_VALUE;
		next->value.kind = QV_GLOBAL;
		next->value.type = ctx->arch.ptr;
		next->value.name = xstrdup(qdef->name);
		next->next = NULL;
		dataitem->next = next;

		qbe_append_def(ctx->out, test);
	}

	ctx->current = NULL;
}

static struct qbe_data_item *
gen_data_item(struct gen_context *ctx, const struct expression *expr,
	struct qbe_data_item *item)
{
	assert(expr->type == EXPR_LITERAL);

	struct qbe_def *def;
	const struct expression_literal *literal = &expr->literal;
	const struct type *type = type_dealias(NULL, expr->result);
	if (type->storage == STORAGE_ENUM) {
		type = type->alias.type;
	}
	type = lower_flexible(NULL, type, NULL);
	if (literal->object) {
		item->type = QD_SYMOFFS;
		item->sym = ident_to_sym(ctx->itbl, literal->object->ident);
		if (type->storage == STORAGE_SLICE) {
			item->offset = literal->slice.offset +
				literal->slice.start * type->array.members->size;

			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_VALUE;
			item->value = constl(literal->slice.len);

			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_VALUE;
			item->value = constl(literal->slice.cap);
		} else {
			item->offset = literal->ival;
		}
		return item;
	}

	switch (type->storage) {
	case STORAGE_I8:
	case STORAGE_U8:
		item->type = QD_VALUE;
		item->value = constw((uint8_t)literal->uval);
		item->value.type = type_is_signed(NULL, type) ? &qbe_sbyte : &qbe_ubyte;
		break;
	case STORAGE_BOOL:
		item->type = QD_VALUE;
		item->value = constw(literal->bval ? 1 : 0);
		item->value.type = &qbe_ubyte;
		break;
	case STORAGE_I16:
	case STORAGE_U16:
		item->type = QD_VALUE;
		item->value = constw((uint16_t)literal->uval);
		item->value.type = type_is_signed(NULL, type) ? &qbe_shalf : &qbe_uhalf;
		break;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
		item->type = QD_VALUE;
		item->value = constw((uint32_t)literal->uval);
		break;
	case STORAGE_U64:
	case STORAGE_I64:
	case STORAGE_SIZE:
		item->type = QD_VALUE;
		item->value = constl((uint64_t)literal->uval);
		break;
	case STORAGE_F32:
		item->type = QD_VALUE;
		item->value = consts((float)literal->fval);
		break;
	case STORAGE_F64:
		item->type = QD_VALUE;
		item->value = constd((double)literal->fval);
		break;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
		item->type = QD_VALUE;
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			item->value = constl((uint64_t)literal->uval);
			break;
		default: assert(0);
		}
		break;
	case STORAGE_ARRAY:
		assert(type->array.length != SIZE_UNDEFINED);
		size_t n = type->array.length;
		for (struct array_literal *c = literal->array;
				c && n; c = c->next ? c->next : c, --n) {
			item = gen_data_item(ctx, c->value, item);
			if (n > 1 || c->next) {
				item->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				item = item->next;
			}
		}
		break;
	case STORAGE_STRING:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(&ctx->id, "strdata.%d");
		def->kind = Q_DATA;
		def->data.align = ALIGN_UNDEFINED;
		def->data.items.type = QD_STRING;
		def->data.items.str = xcalloc(expr->literal.string.len, 1);
		def->data.items.sz = expr->literal.string.len;
		memcpy(def->data.items.str, expr->literal.string.value,
			expr->literal.string.len);

		item->type = QD_VALUE;
		if (expr->literal.string.len != 0) {
			qbe_append_def(ctx->out, def);
			item->value.kind = QV_GLOBAL;
			item->value.type = ctx->arch.ptr;
			item->value.name = xstrdup(def->name);
		} else {
			free(def);
			item->value = constl(0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(expr->literal.string.len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(expr->literal.string.len);
		break;
	case STORAGE_SLICE:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(&ctx->id, "sldata.%d");
		def->kind = Q_DATA;
		def->data.align = ALIGN_UNDEFINED;

		if (literal->slice.len != 0) {
			struct qbe_data_item *subitem = &def->data.items;
			for (struct array_literal *c = literal->slice.array;
					c; c = c->next) {
				subitem = gen_data_item(ctx, c->value, subitem);
				if (c->next) {
					subitem->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					subitem = subitem->next;
				}
			}
			qbe_append_def(ctx->out, def);
			item->type = QD_SYMOFFS;
			item->sym = xstrdup(def->name);
			item->offset =
				literal->slice.start * type->array.members->size;
		} else {
			item->type = QD_VALUE;
			item->value = constl(0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(literal->slice.len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(literal->slice.cap);
		break;
	case STORAGE_STRUCT:
		for (struct struct_literal *f = literal->_struct;
				f; f = f->next) {
			if (f->field->type->size != 0) {
				item = gen_data_item(ctx, f->value, item);
			}
			if (f->next) {
				const struct struct_field *f1 = f->field;
				const struct struct_field *f2 = f->next->field;
				if (f2->offset > f1->offset + f1->type->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = f2->offset -
						(f1->offset + f1->type->size);
				}

				if (f->field->type->size != 0) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
				}
			} else {
				const struct struct_field *fi = f->field;
				if (fi->offset + fi->type->size
						!= expr->result->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = expr->result->size
						- (fi->offset + fi->type->size);
				}
			}
		}
		break;
	case STORAGE_TUPLE:
		for (const struct tuple_literal *tuple = literal->tuple;
				tuple; tuple = tuple->next) {
			if (tuple->field->type->size != 0) {
				item = gen_data_item(ctx, tuple->value, item);
			}
			if (tuple->next) {
				const struct type_tuple *f1 = tuple->field;
				const struct type_tuple *f2 = tuple->next->field;
				if (f2->offset > f1->offset + f1->type->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = f2->offset -
						(f1->offset + f1->type->size);
				}

				if (tuple->field->type->size != 0) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
				}
			} else {
				const struct type_tuple *fi = tuple->field;
				if (fi->offset + fi->type->size
						!= expr->result->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = expr->result->size
						- (fi->offset + fi->type->size);
				}
			}
		}
		break;
	case STORAGE_TAGGED:
		item->type = QD_VALUE;
		item->value = constw((uint32_t)literal->tagged.tag->id);
		size_t offs = builtin_type_u32.size;
		size_t tag_align = literal->tagged.tag->align;
		if (tag_align > offs) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_ZEROED;
			item->zeroed = tag_align - offs;
			offs = tag_align;
		}
		if (literal->tagged.tag->size != 0) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item = gen_data_item(ctx, literal->tagged.value, item);
			offs += literal->tagged.tag->size;
		}
		if (offs < type->size) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_ZEROED;
			item->zeroed = type->size - offs;
		}
		break;
	case STORAGE_DONE:
	case STORAGE_NOMEM:
	case STORAGE_VOID:
		break;
	case STORAGE_ENUM:
	case STORAGE_UNION:
	case STORAGE_ALIAS:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_RCONST:
	case STORAGE_NULL:
	case STORAGE_VALIST:
		assert(0); // Invariant
	}

	assert(item->type != QD_VALUE || item->value.type);
	return item;
}

static void
gen_global_decl(struct gen_context *ctx, const struct declaration *decl)
{
	assert(decl->decl_type == DECL_GLOBAL);
	const struct global_decl *global = &decl->global;
	if (!global->value) {
		return; // Forward declaration
	}
	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_DATA;
	qdef->data.align = ALIGN_UNDEFINED;
	qdef->data.threadlocal = global->threadlocal;
	qdef->exported = decl->exported;
	qdef->name = decl->symbol ? decl->symbol : ident_to_sym(ctx->itbl, decl->ident);
	qdef->file = decl->file;
	gen_data_item(ctx, global->value, &qdef->data.items);
	qbe_append_def(ctx->out, qdef);
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->decl_type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		gen_global_decl(ctx, decl);
		break;
	case DECL_TYPE:
	case DECL_CONST:
		break; // no-op
	}
}

void
gen(const struct unit *unit, struct qbe_program *out, struct intern_table *itbl)
{
	struct gen_context ctx = {
		.out = out,
		.ns = unit->ns,
		.itbl = itbl,
		.arch = {
			.ptr = &qbe_long,
			.sz = &qbe_long,
		},
	};
	ctx.out->next = &ctx.out->defs;
	rtfunc_init(&ctx);

	ctx.sources = xcalloc(nsources + 1, sizeof(struct gen_value));
	for (size_t i = 1; i <= nsources; i++) {
		struct expression eloc;
		mkstrliteral(&eloc, "%s", sources[i]);
		ctx.sources[i] = gen_literal_string(&ctx, &eloc);
	}

	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, &decls->decl);
		decls = decls->next;
	}
}
