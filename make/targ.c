/*	$OpenBSD: targ.c,v 1.88 2024/06/18 02:11:03 millert Exp $ */
/*	$NetBSD: targ.c,v 1.11 1997/02/20 16:51:50 christos Exp $	*/

/*
 * Copyright (c) 1999 Marc Espie.
 *
 * Extensive code changes for the OpenBSD project.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE OPENBSD PROJECT AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENBSD
 * PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*-
 * targ.c --
 *		Target nodes are kept into a hash table.
 *
 * Interface:
 *	Targ_Init		Initialization procedure.
 *
 *	Targ_NewGN		Create a new GNode for the passed target
 *				(string). The node is *not* placed in the
 *				hash table, though all its fields are
 *				initialized.
 *
 *	Targ_FindNode		Find the node for a given target, creating
 *				and storing it if it doesn't exist and the
 *				flags are right (TARG_CREATE)
 *
 *	Targ_FindList		Given a list of names, find nodes for all
 *				of them, creating nodes if needed.
 *
 *	Targ_Ignore		Return true if errors should be ignored when
 *				creating the given target.
 *
 *	Targ_Silent		Return true if we should be silent when
 *				creating the given target.
 *
 *	Targ_Precious		Return true if the target is precious and
 *				should not be removed if we are interrupted.
 */

#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ohash.h>
#include "defines.h"
#include "stats.h"
#include "suff.h"
#include "var.h"
#include "targ.h"
#include "memory.h"
#include "gnode.h"
#include "extern.h"
#include "timestamp.h"
#include "lst.h"
#include "node_int.h"
#include "nodehashconsts.h"
#include "dump.h"

static struct ohash targets;	/* hash table of targets */
struct ohash_info gnode_info = {
	offsetof(GNode, name), NULL, hash_calloc, hash_free, element_alloc
};

static GNode *Targ_mk_node(const char *, const char *, unsigned int,
    unsigned char, unsigned int);

#define Targ_mk_constant(n, type) \
    Targ_mk_special_node(n, sizeof(n), K_##n, type, SPECIAL_NONE, 0)

static GNode *
Targ_CreateNodeFromPattern(GNode *pattern_gn, char *pattern_value,
	size_t pattern_value_len);

static void
Targ_BuildChildFromPatternParent(GNode *parent_gn, GNode *child, char *pattern_value,
	size_t pattern_value_len);

static char *expand_from_char(const char *src, size_t src_len, const char *to_expand,
	size_t to_expand_len, const char *pattern_value, size_t pattern_value_len);

static char *expand_pattern_from_char(const char *src, size_t src_len,
	const char *pattern_value, size_t pattern_value_len);

static void Targ_RemoveTmpTarg(void *child, void *unused UNUSED);

GNode *begin_node, *end_node, *interrupt_node, *DEFAULT;

void
Targ_Init(void)
{
	/* A small make file already creates 200 targets.  */
	ohash_init(&targets, 10, &gnode_info);
	begin_node = Targ_mk_constant(NODE_BEGIN, 
	    OP_DUMMY | OP_NOTMAIN | OP_NODEFAULT);
	end_node = Targ_mk_constant(NODE_END, 
	    OP_DUMMY | OP_NOTMAIN | OP_NODEFAULT);
	interrupt_node = Targ_mk_constant(NODE_INTERRUPT,
	    OP_DUMMY | OP_NOTMAIN | OP_NODEFAULT);
	DEFAULT = Targ_mk_constant(NODE_DEFAULT, 
	    OP_DUMMY | OP_NOTMAIN| OP_TRANSFORM | OP_NODEFAULT);

}

static GNode *
Targ_mk_node(const char *name, const char *ename, 
    unsigned int type, unsigned char special, unsigned int special_op)
{
	GNode *gn;

	gn = ohash_create_entry(&gnode_info, name, &ename);
	gn->path = NULL;
	gn->type = type;
	gn->special = special;
	gn->special_op = special_op;
	gn->children_left = 0;
	gn->must_make = false;
	gn->built_status = UNKNOWN;
	gn->in_cycle = false;
	gn->child_rebuilt = false;
	gn->order = 0;
	ts_set_out_of_date(gn->mtime);
	gn->youngest = gn;
	Lst_Init(&gn->cohorts);
	Lst_Init(&gn->parents);
	Lst_Init(&gn->children);
	Lst_Init(&gn->predecessors);
	Lst_Init(&gn->successors);
	SymTable_Init(&gn->localvars);
	gn->impliedsrc = NULL;
	Lst_Init(&gn->commands);
	gn->suffix = NULL;
	gn->next = NULL;
	gn->basename = NULL;
	gn->sibling = gn;
	gn->groupling = NULL;
	gn->is_pattern = (strchr(name, '%') != NULL);
	gn->is_tmp = gn->is_pattern;
	gn->pattern_value = NULL;

#ifdef STATS_GN_CREATION
	STAT_GN_COUNT++;
#endif

	return gn;
}

GNode *
Targ_NewGNi(const char *name, const char *ename)
{
	return Targ_mk_node(name, ename, OP_ZERO, SPECIAL_NONE, 0);
}

/*
 * Expands 'to_expand' in src with pattern_value
 * Returns a newly allocated string that must be freed by the caller.
 * Returns NULL on error.
 */
static char *
expand_from_char(const char *src, size_t src_len, const char *to_expand,
	size_t to_expand_len, const char *pattern_value, size_t pattern_value_len)
{
	const char *expander;
	char *result;
	size_t prefix_len;
	size_t suffix_len;
	size_t result_len;

	if (!src || !pattern_value || !to_expand)
		return NULL;

	expander = strstr(src, to_expand);
	if (!expander) {
		/* No match, return a copy of src */
		return strndup(src, src_len);
	}

	prefix_len = expander - src;
	suffix_len = src_len - prefix_len - to_expand_len;
	result_len = prefix_len + pattern_value_len + suffix_len;
	
	result = emalloc(result_len + 1);
	memcpy(result, src, prefix_len);
	memcpy(result + prefix_len, pattern_value, pattern_value_len);
	memcpy(result + prefix_len + pattern_value_len, 
	    expander + to_expand_len, suffix_len);
	result[result_len] = '\0';

	return result;
}

/*
 * Expands pattern '%' in src with pattern_value
 */
char *
expand_pattern_from_char(const char *src, size_t src_len, 
    const char *pattern_value, size_t pattern_value_len)
{
	return expand_from_char(src, src_len, "%", 1, 
	    pattern_value, pattern_value_len);
}

static void
Targ_CopyCommands(GNode *dest, GNode *src)
{
	LstNode ln;
	struct command *cmd;
	const char *cmd_str;
	size_t cmd_len, elen;
	struct command *new_cmd;

	for (ln = Lst_First(&src->commands); ln != NULL; ln = Lst_Adv(ln)) {
		cmd = Lst_Datum(ln);
		cmd_str = cmd->string;
		cmd_len = strlen(cmd_str);
		elen = cmd_len;

		new_cmd = emalloc(offsetof(struct command, string) + elen + 1);
		new_cmd->location = cmd->location;
		memcpy(new_cmd->string, cmd_str, elen + 1);

		Lst_AtEnd(&dest->commands, new_cmd);
	}
}

/*-
 *-----------------------------------------------------------------------
 * Targ_CreateNodeFromPattern --
 *	Create a new GNode from a pattern node by expanding the pattern
 *	with pattern_value. It only creates the node and copies commands,
 *	it does not link it to any parent or create its children.
 *
 * Returns:
 *	The newly created GNode, or NULL on error.
 *-----------------------------------------------------------------------
 */
static GNode *
Targ_CreateNodeFromPattern(GNode *pattern_gn, char *pattern_value,
    size_t pattern_value_len)
{
	char *new_name;
	GNode *new_gn;
	unsigned int slot;
	const char *ename;
	uint32_t hv;

	if (DEBUG(PATTERN))
		printf("Building children %s ", pattern_gn->name);

	/* First compute expanded name. */
	new_name = expand_pattern_from_char(pattern_gn->name,
	    strlen(pattern_gn->name), pattern_value, pattern_value_len);
	if (new_name == NULL) {
		printf("Targ_BuildFromPattern: ERROR: expand_pattern_from_char failed.\n");
		return NULL;
	}

	if (DEBUG(PATTERN))
		printf("(new name: %s)\n", new_name);

	/* Create new GNode with expanded name. */
	new_gn = Targ_NewGNi(new_name, new_name + strlen(new_name));
	if (new_gn == NULL) {
		if (DEBUG(PATTERN))
			printf("Targ_BuildFromPattern: ERROR: node creation failed.\n");
		free(new_name);
		return NULL;
	}
	
	new_gn->expanded_from = pattern_gn;
	new_gn->is_tmp = true;

	/* Search its place in ohash and insert it. */
	ename = new_name + strlen(new_name);
	hv = ohash_interval(new_name, &ename);
	slot = ohash_lookup_interval(&targets, new_name, ename, hv);
	ohash_insert(&targets, slot, new_gn);

	free(new_name);

	/* Copy commands from pattern_gn to new_gn. */
	Targ_CopyCommands(new_gn, pattern_gn);

	return new_gn;
}

static void
Targ_BuildChildFromPatternParent(GNode *parent_gn, GNode *child,
    char *pattern_value, size_t pattern_value_len)
{
	GNode *new_child;

	/* Create new node from pattern. */
	new_child = Targ_CreateNodeFromPattern(child, pattern_value,
	    pattern_value_len);
	if (new_child == NULL) {
		printf("Targ_BuildChildFromPatternParent: ERROR: node creation failed.\n");
		return;
	}

	/* Recursively build and link all children from the pattern. */
	Targ_BuildFromPattern(new_child, child, pattern_value,
	    pattern_value_len);

	new_child->is_pattern = false;
	new_child->pattern_value = strndup(pattern_value, pattern_value_len);

	/* Link new_child to parent_gn and vice-versa. */
	Lst_AtEnd(&parent_gn->children, new_child);
	Lst_AtEnd(&new_child->parents, parent_gn);

	parent_gn->children_left++;
}

/*-
 *-----------------------------------------------------------------------
 * Targ_BuildFromPattern --
 *	Build a new GNode from a pattern node by expanding the pattern
 *	with pattern_value. This function creates the new node, builds
 *	its children recursively, and links it to parent_gn.
 *-----------------------------------------------------------------------
 */
void
Targ_BuildFromPattern(GNode *parent_gn, GNode *pattern_gn, char *pattern_value,
    size_t pattern_value_len)
{
	LstNode ln;
	GNode *child;

	if (!parent_gn || !pattern_gn || !pattern_value ||
	    pattern_value_len == 0) {
		if (DEBUG(PATTERN))
			printf("Targ_BuildFromPattern: ERROR: invalid parameters.\n");
		return;
	}

	if (DEBUG(PATTERN))
		printf("Building children of %s from %s with %%=%.*s\n",
		    parent_gn->name, pattern_gn->name,
		    (int)pattern_value_len, pattern_value);

	/* For each child of pattern_gn, create a new child for parent_gn
	 * replacing % with pattern_value. */
	for (ln = Lst_First(&pattern_gn->children); ln != NULL;
	    ln = Lst_Adv(ln)) {
		child = Lst_Datum(ln);
		Targ_BuildChildFromPatternParent(parent_gn, child,
		    pattern_value, pattern_value_len);
	}

	/* Copy commands from pattern_gn to parent_gn.
	 * It works because we are on a node without children (called
	 * from "expand_children_from").
	 * This means that current node does not have its own commands
	 * so we can safely copy commands from pattern_gn. */
	Targ_CopyCommands(parent_gn, pattern_gn);
}

GNode *
Targ_FindNodei(const char *name, const char *ename, int flags)
{
	uint32_t hv;
	GNode *gn;
	unsigned int slot;

	hv = ohash_interval(name, &ename);

	slot = ohash_lookup_interval(&targets, name, ename, hv);

	gn = ohash_find(&targets, slot);

	if (gn == NULL && (flags & TARG_CREATE)) {
		gn = Targ_NewGNi(name, ename);
		ohash_insert(&targets, slot, gn);
	}

	return gn;
}

/*-
 *-----------------------------------------------------------------------
 * match_pattern --
 *	Match a name against a pattern, expanding any wildcards.
 *	If a match is found, return true and set *expanded to the expanded
 *	portion of the name corresponding to the `%` in the pattern.
 *	If no match is found, return false.
 *	If the expanded portion is not needed, set *expanded to NULL.
 *
 *	WARNING: The caller is responsible for freeing *expanded when
 *	it is no longer needed.
 *
 *	WARNING 2: This function only supports a single `%` wildcard.
 *  If multiple wildcards are present, only the first one is considered.
 *-----------------------------------------------------------------------
 */
bool
match_pattern(const char *name, const char *pattern, char **expanded)
{
	const char *p = pattern;
	const char *n = name;
	const char *wildcard = NULL;	/* Position of `%` in pattern */
	const char *match_start = NULL;	/* Corresponding position in name */
	bool result;
	size_t len;

	if (!name || !pattern)
		return false;

	while (*n && *p) {
		if (*p == *n) {
			/* Advance if characters match. */
			p++;
			n++;
		} else if (*p == '%') {
			/* If we find a `%`, record its position. */
			wildcard = p;
			match_start = n;
			p++;	/* Skip the `%` */
		} else if (wildcard) {
			/* If a `%` has already been encountered, try to
			 * consume `name`. */
			match_start++;		/* Try the next position */
			n = match_start;
			p = wildcard + 1;	/* Resume after the `%` */
		} else {
			return false;		/* No possible match */
		}
	}

	/* Check that the rest of `pattern` is only `%`. */
	while (*p == '%')
		p++;

	result = (*p == '\0');	/* If we have consumed all of `pattern` */

	if (result && wildcard && expanded) {
		len = match_start - name;
		*expanded = strndup(name, len);
	}

	return result;
}

/*-
 *-----------------------------------------------------------------------
 * Targ_FindPatternMatchingNode --
 *	Take a name as parameter and search for all ->is_pattern targets
 *	whose ->name match with the parameter.
 *
 * Returns:
 *	The matching GNode, or NULL if no match found.
 *-----------------------------------------------------------------------
 */
GNode *
Targ_FindPatternMatchingNode(const GNode *gnode_from, char **expanded)
{
	const char *name = gnode_from->name;
	GNode *gn;
	unsigned int i;
	bool is_parent;
	LstNode ln;
	GNode *parent;

	/* Iterate over each target in the table.
	 * If the target is a pattern and the target's name matches the name
	 * passed as a parameter, return the target. */
	gn = NULL;
	i = 0;
	for (gn = ohash_first(&targets, &i); gn != NULL;
	    gn = ohash_next(&targets, &i)) {
		if (gn->is_pattern && (strcmp(name, gn->name) != 0) &&
		    match_pattern(name, gn->name, expanded)) {

			/* Check if gn not a parent of name. */
			is_parent = false;
			if (gnode_from->expanded_from == gn) {
				is_parent = true;
			} else {
				/* Check parents. */
				ln = Lst_First(&gn->parents);
				for (parent = NULL; ln != NULL;
				    ln = Lst_Adv(ln)) {
					parent = Lst_Datum(ln);

					if (DEBUG(PATTERN))
						printf("\t - name = %s, parent_name = %s\n",
						    name, parent->name);

					if (strcmp(name, parent->name) == 0) {
						is_parent = true;
						break;
					}
				}
			}

			if (!is_parent && gn)
				return gn;
		}
	}
	return NULL;
}

static void
Targ_RemoveTmpTarg(void *child, void *unused UNUSED)
{
	GNode *gn = child;
	const char *file;

	if (!gn || !gn->expanded_from || !gn->is_tmp)
		return;

	if (DEBUG(PATTERN))
		printf("Targ_RemoveTmpTarg: Removing node %s\n", gn->name);

	file = gn->path != NULL ? gn->path : gn->name;
	if (eunlink(file) == 0) {
		if (!Targ_Silent(gn))
			printf("rm %s\n", file);
		gn->is_tmp = false;
	} else {
		fprintf(stderr, "*** couldn't delete %s: %s\n", file,
		    strerror(errno));
	}
}

void
Targ_RemoveAllTmpChildren(GNode *gn)
{
	Lst_ForEach(&gn->children, Targ_RemoveTmpTarg, NULL);
}

GNode *
Targ_mk_special_node(const char *name, size_t n, uint32_t hv,
    unsigned int type, unsigned char special, unsigned int special_op)
{
	GNode *gn;
	unsigned int slot;
	const char *ename = name + n - 1;

	slot = ohash_lookup_interval(&targets, name, ename, hv);

	assert(ohash_find(&targets, slot) == NULL);

	gn = Targ_mk_node(name, ename, type, special, special_op);
	ohash_insert(&targets, slot, gn);

	return gn;
}

void
Targ_FindList(Lst nodes, Lst names)
{
	LstNode ln;
	GNode *gn;
	char *name;

	for (ln = Lst_First(names); ln != NULL; ln = Lst_Adv(ln)) {
		name = Lst_Datum(ln);
		gn = Targ_FindNode(name, TARG_CREATE);
		/* Note: Lst_AtEnd must come before the Lst_Concat so the nodes
		 * are added to the list in the order in which they were
		 * encountered in the makefile.  */
		Lst_AtEnd(nodes, gn);
		if (gn->type & OP_DOUBLEDEP)
			Lst_Concat(nodes, &gn->cohorts);
	}
}

bool
Targ_Ignore(GNode *gn)
{
	if (ignoreErrors || gn->type & OP_IGNORE)
		return true;
	else
		return false;
}

bool
Targ_Silent(GNode *gn)
{
	if (beSilent || gn->type & OP_SILENT)
		return true;
	else
		return false;
}

bool
Targ_Precious(GNode *gn)
{
	if (allPrecious || (gn->type & (OP_PRECIOUS|OP_DOUBLEDEP|OP_PHONY)))
		return true;
	else
		return false;
}

bool
node_is_real(GNode *gn)
{
	return (gn->type & OP_DUMMY) == 0;
}

void
Targ_PrintCmd(void *p)
{
	const struct command *cmd = p;
	printf("\t%s\n", cmd->string);
}

void
Targ_PrintType(int type)
{
	int    tbit;

#define PRINTBIT(attr)	case CONCAT(OP_,attr): printf("." #attr " "); break
#define PRINTDBIT(attr) case CONCAT(OP_,attr): if (DEBUG(TARG)) printf("." #attr " "); break

	type &= ~OP_OPMASK;

	while (type) {
		tbit = 1 << (ffs(type) - 1);
		type &= ~tbit;

		switch (tbit) {
		PRINTBIT(OPTIONAL);
		PRINTBIT(USE);
		PRINTBIT(IGNORE);
		PRINTBIT(PRECIOUS);
		PRINTBIT(SILENT);
		PRINTBIT(MAKE);
		PRINTBIT(INVISIBLE);
		PRINTBIT(NOTMAIN);
		/*XXX: MEMBER is defined, so CONCAT(OP_,MEMBER) gives OP_"%" */
		case OP_MEMBER:
			if (DEBUG(TARG))
				printf(".MEMBER ");
			break;
		PRINTDBIT(ARCHV);
		}
    }
}

const char *
status_to_string(GNode *gn)
{
	switch (gn->built_status) {
	case UNKNOWN:
		return "unknown";
	case REBUILT:
		return "made";
	case UPTODATE:
		return "up-to-date";
	case ERROR:
		return "error when made";
	case ABORTED:
		return "aborted";
	default:
		return "other status";
	}
}

struct ohash *
targets_hash(void)
{
	return &targets;
}
