#ifndef HAREC_SCOPE_H
#define HAREC_SCOPE_H
#include "expr.h"
#include "identifier.h"

#define SCOPE_BUCKETS 4096

enum object_type {
	O_BIND,
	O_CONST,
	O_DECL,
	O_SCAN,
	O_TYPE,
};

enum scope_object_flags {
	SO_THREADLOCAL = 1 << 0,
	SO_FOR_EACH_SUBJECT = 1 << 1,
};

struct scope_object {
	enum object_type otype;
	// name is the name of the object within this scope (for lookups)
	// ident is the global identifier (these may be different in some cases)
	struct ident *name;
	struct ident *ident;
	enum scope_object_flags flags;

	union {
		const struct type *type;
		struct expression *value; // For O_CONST
	};
	 // Cannot be in union because type and idecl are needed at the same time
	struct incomplete_decl *idecl;

	struct scope_object *lnext; // Linked list
	struct scope_object *mnext; // Hash map
};

enum scope_class {
	SCOPE_COMPOUND,
	SCOPE_DEFER,
	SCOPE_ENUM,
	SCOPE_FUNC,
	SCOPE_LOOP,
	SCOPE_MATCH,
	SCOPE_SUBUNIT,
	SCOPE_UNIT,
	SCOPE_DEFINES,
};

struct yield {
	struct expression **expression;
	struct yield *next;
};

struct scope {
	// Used for for loops
	bool has_break;

	enum scope_class class;
	const char *label;
	struct scope *parent;

	const struct type *hint;
	struct type_tagged_union *results;
	struct yield *yields;

	// Linked list in insertion order
	// Used for function parameters and enum values, where order matters
	struct scope_object *objects;
	struct scope_object **next;

	// Hash map in reverse insertion order
	// Used for lookups, and accounts for shadowing
	struct scope_object *buckets[SCOPE_BUCKETS];
};

struct scopes {
	struct scope *scope;
	struct scopes *next;
};

struct scope *scope_push(struct scope **stack, enum scope_class class);
struct scope *scope_pop(struct scope **stack);

struct scope *scope_lookup_class(struct scope *scope, enum scope_class class);
struct scope *scope_lookup_label(struct scope *scope, const char *label);

void scope_free(struct scope *scope);
void scope_free_all(struct scopes *scopes);

struct scope_object *scope_insert(struct scope *scope,
	enum object_type otype, struct ident *ident, struct ident *name,
	const struct type *type, struct expression *value);

struct scope_object *scope_lookup(struct scope *scope, struct ident *ident);

#endif
