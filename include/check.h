#ifndef HARE_CHECK_H
#define HARE_CHECK_H
#include <stdbool.h>
#include <stdnoreturn.h>
#include "ast.h"
#include "identifier.h"
#include "scope.h"
#include "types.h"
#include "type_store.h"
#include "util.h"

struct expression;

#define MODCACHE_BUCKETS 256

struct modcache {
	struct ident *ident;
	struct scope *scope;
	struct modcache *next;
};

struct errors {
	struct location loc;
	char *msg;
	struct errors *next;
};

struct context {
	type_store *store;
	struct modcache **modcache;
	const struct type *fntype;
	struct ident *ns;
	struct scope *unit;
	struct scope *scope;
	struct scope *defines;
	const char *mainsym;
	struct ident *mainident;
	bool is_test;
	int id;
	struct errors *errors;
	struct errors **next;
	struct declarations *decls;
	struct ast_types *unresolved;
	struct intern_table *itbl;
};

struct constant_decl {
	const struct type *type;
	const struct expression *value;
};

struct function_decl {
	const struct type *type;
	struct expression *body;
	struct scope *scope;
	unsigned int flags; // enum func_decl_flags
};

struct global_decl {
	const struct type *type;
	struct expression *value; // EXPR_LITERAL
	bool threadlocal;
};

enum decl_type {
	DECL_FUNC,
	DECL_TYPE,
	DECL_GLOBAL,
	DECL_CONST,
};

struct declaration {
	enum decl_type decl_type;
	int file;
	struct ident *ident;
	const char *symbol;
	bool exported; // XXX: this bool takes up 8 bytes and i am in pain
	union {
		struct constant_decl constant;
		struct function_decl func;
		struct global_decl global;
		const struct type *type;
	};
};

struct declarations {
	struct declaration decl;
	struct declarations *next;
};

struct unit {
	struct ident *ns;
	struct declarations *declarations;
	struct identifiers *imports;
};

enum idecl_type {
	IDECL_DECL,
	IDECL_ENUM_FLD,
};

// Keeps track of enum specific context required for enum field resolution
struct incomplete_enum_field {
	struct ast_enum_field *field;
	struct scope *enum_scope;
};

// Keeps track of context required to resolve a declaration or an enum field
// Extends the scope_object struct so it can be inserted into a scope
struct incomplete_decl {
	struct scope *imports; // the scope of this declaration's subunit
	enum idecl_type type;
	bool in_progress;
	bool dealias_in_progress;
	union {
		struct ast_decl decl;
		struct incomplete_enum_field *field;
	};
};

struct ident *mkident(struct context *ctx, struct ident *ident,
		const char *symbol);

void append_decl(struct context *ctx, struct declaration *decl);

void mkstrliteral(struct expression *expr, const char *fmt, ...) FORMAT(2, 3);

char *gen_typename(const struct type *type);

struct expression *lower_implicit_cast(struct context *ctx,
		const struct type *to, struct expression *expr);

typedef void (*resolvefn)(struct context *, struct scope_object *obj);

void resolve_dimensions(struct context *ctx, struct scope_object *obj);

void resolve_type(struct context *ctx, struct scope_object *obj);

void wrap_resolver(struct context *ctx,
	struct scope_object *obj, resolvefn resolver);

struct scope *check(type_store *ts,
	bool is_test,
	const char *mainsym,
	struct ident *mainident,
	const struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit,
	struct intern_table *itbl);

struct scope *check_internal(type_store *ts,
	struct modcache **cache,
	bool is_test,
	const char *mainsym,
	struct ident *mainident,
	const struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit,
	struct intern_table *itbl,
	bool scan_only);

void check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint);

void error(struct context *ctx, struct location loc,
	struct expression *expr, const char *fmt, ...) FORMAT(4, 5);
#endif
