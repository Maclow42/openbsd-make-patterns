/*	$OpenBSD$ */

/*
 * Copyright (c) 2025 Thibault Colcomb <thibault.colcomb@epita.fr>
 * Copyright (c) 2025 Marc Espie <espie@openbsd.org>
 *
 * Support for gnu-make style % patterns
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

/*-
 * 	Pattern nodes are stored in a separate hash table,
 *	and cloned when recognized
 */

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ohash.h>
#include "defines.h"
#include "dir.h"
#include "lst.h"
#include "gnode.h"
#include "targ.h"
#include "memory.h"
#include "str.h"
#include "patterns.h"
#include "garray.h"

static struct growableArray patterns;

void
may_register_as_pattern(GNode *gn)
{
	if (strchr(gn->name, '%') == NULL) {
		gn->is_tmp = false;
		return;
	}
	Array_AtEnd(&patterns, gn);
}

static GNode *
Targ_CreateNodeFromPattern(GNode *, char *, size_t);

static void
Targ_BuildChildFromPatternParent(GNode *, GNode *, char *, size_t);

static char *expand_from_char(const char *, size_t,  const char *,
	size_t, const char *, size_t);

static char *expand_pattern_from_char(const char *, size_t ,
	const char *, size_t);

static void Targ_RemoveTmpTarg(void *, void *UNUSED);

void
Pattern_Init(void)
{
	Array_Init(&patterns, 32);
}

char *
find_file_hash_with_pattern(struct ohash *h, const char *pattern)
{
	unsigned int search;
	const char *entry;

	for (entry = ohash_first(h, &search); entry != NULL;
	    entry = ohash_next(h, &search)) {
		if (!match_pattern(entry, pattern, NULL)) {
			if (DEBUG(PATTERN))
				printf("find_file_hash_with_pattern: "
				    "Matched file: %s\n",
				    entry);

			return estrdup(entry);
		}
	}
	return NULL;
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

	expander = strstr(src, to_expand);
	if (!expander) {
		/* No match, return a copy of src */
		return Str_dupi(src, src + src_len);
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
	struct ohash *h;

	if (DEBUG(PATTERN))
		printf("Building children %s ", pattern_gn->name);

	/* First compute expanded name. */
	new_name = expand_pattern_from_char(pattern_gn->name,
	    strlen(pattern_gn->name), pattern_value, pattern_value_len);

	if (DEBUG(PATTERN))
		printf("(new name: %s)\n", new_name);

	ename = new_name + strlen(new_name);
	/* Create new GNode with expanded name. */
	new_gn = Targ_NewGNi(new_name, ename);

	new_gn->expanded_from = pattern_gn;
	new_gn->is_tmp = true;

	/* XXX Should not exist ? */

	/* Search its place in ohash and insert it. */
	hv = ohash_interval(new_name, &ename);
	h = targets_hash();
	slot = ohash_lookup_interval(h, new_name, ename, hv);
	ohash_insert(h, slot, new_gn);

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

	/* Recursively build and link all children from the pattern. */
	Targ_BuildFromPattern(new_child, child, pattern_value,
	    pattern_value_len);

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

	/* Note: pattern_value_len can be 0 for empty stems (e.g., "lib%.a"
	 * matching "lib.a"), which is valid. */

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

/*-
 *-----------------------------------------------------------------------
 * match_pattern --
 *	Match a name against a pattern with a single '%' wildcard.
 *	The pattern is split into prefix and suffix around the '%'.
 *	Example: "lib%.a" matches "libfoo.a" with expanded = "foo"
 *
 *	If a match is found, return true and optionally set *expanded
 *	to the portion of name that matched the '%' wildcard.
 *
 * Returns:
 *	true if pattern matches name, false otherwise.
 *
 * Side Effects:
 *	If expanded is non-NULL and pattern matches, *expanded is set
 *	to a newly allocated string containing the wildcard match.
 *	Caller must free *expanded when done.
 *-----------------------------------------------------------------------
 */
bool
match_pattern(const char *name, const char *pattern, char **expanded)
{
	const char *percent;
	const char *name_ptr;
	size_t prefix_len;
	size_t suffix_len;
	size_t name_len;
	size_t stem_len;

	/* Find the '%' in pattern. */
	percent = strchr(pattern, '%');
	if (percent == NULL) {
		/* No wildcard, must match exactly. */
		return strcmp(name, pattern) == 0;
	}

	/* Calculate prefix and suffix lengths. */
	prefix_len = percent - pattern;
	suffix_len = strlen(percent + 1);
	name_len = strlen(name);

	/* Check if name is long enough to contain prefix and suffix. */
	if (name_len < prefix_len + suffix_len)
		return false;

	/* Check prefix matches. */
	if (prefix_len > 0 && strncmp(name, pattern, prefix_len) != 0)
		return false;

	/* Check suffix matches. */
	if (suffix_len > 0) {
		name_ptr = name + name_len - suffix_len;
		if (strcmp(name_ptr, percent + 1) != 0)
			return false;
	}

	/* Pattern matches. Extract stem if requested. */
	if (expanded != NULL) {
		stem_len = name_len - prefix_len - suffix_len;
		/* Note: stem_len can be 0 for patterns like "lib%.a" matching
		 * "lib.a", which is valid and should return empty string. */
		*expanded = strndup(name + prefix_len, stem_len);
		if (*expanded == NULL) {
			/* Memory allocation failed. */
			fprintf(stderr, "match_pattern: out of memory\n");
			return false;
		}
	}

	return true;
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
	bool is_parent;
	char *temp_expanded;
	LstNode ln;
	size_t i;
	/* If no pattern gnode exists  we do not try to search one */
	if (Array_IsEmpty(&patterns))
		return NULL;

	/* Iterate over each pattern, looking for a match */
	gn = NULL;
	for (i = 0; i != patterns.n; i++) {
		gn = patterns.a[i];
		temp_expanded = NULL;
		if ((strcmp(name, gn->name) != 0) &&
		    match_pattern(name, gn->name, &temp_expanded)) {

			/* Check if gn not a parent of name. */
			is_parent = false;
			if (gnode_from->expanded_from == gn) {
				is_parent = true;
			}

			if (!is_parent && gn) {
				/* Transfer ownership to caller */
				if (expanded != NULL)
					*expanded = temp_expanded;
				else
					free(temp_expanded);
				return gn;
			}

			/* Pattern matched but is a parent, free and continue */
			free(temp_expanded);
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
	/* Tmp children are only created by pattern rules. */
	if (Array_IsEmpty(&patterns))
		return;
	Lst_ForEach(&gn->children, Targ_RemoveTmpTarg, NULL);
}


bool
expand_children_from_pattern(GNode *gn)
{
	GNode *matching;
	char *expanded = NULL;

	if (DEBUG(PATTERN)) {
		printf("\t\t => No children found for \"%s\"\n", gn->name);
		printf("\t\t => Searching for matching pattern targets...\n");
	}

	matching = Targ_FindPatternMatchingNode(gn, &expanded);
	if (matching != NULL) {
		if (DEBUG(PATTERN))
			printf("\t\t => Matching pattern target found: %s\n",
			    matching->name);

		/* Replace all % pattern of matching node with parent
		 * node and add it to the parent children list. */
		Targ_BuildFromPattern(gn, matching, expanded, strlen(expanded));

		free(expanded);

		if (DEBUG(PATTERN))
			printf("All children of %s have been built.\n\n",
			    gn->name);

		return true;
	}

	if (DEBUG(PATTERN))
		printf("\tNo pattern matching found for %s\n", gn->name);

	/* If no children found, the node should exist. */
	gn->is_tmp = false;
	return false;
}
