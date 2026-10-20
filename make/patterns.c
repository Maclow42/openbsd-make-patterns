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
#include "extern.h"
#include "dir.h"
#include "error.h"
#include "lst.h"
#include "gnode.h"
#include "targ.h"
#include "memory.h"
#include "str.h"
#include "timestamp.h"
#include "patterns.h"
#include "garray.h"

/* A registered pattern rule, with its '%' split precomputed. */
struct pattern {
	GNode *gn;
	size_t prefix_len;	/* length of the name before '%' */
	const char *suffix;	/* name after '%' */
	size_t suffix_len;
};

static struct pattern *patterns;
static size_t npatterns, maxpatterns;
static bool tmp_made;	/* a temporary target was made after all */

static GNode *
Targ_CreateNodeFromPattern(GNode *, const char *, const char *);

static void
Targ_BuildChildFromPatternParent(GNode *, GNode *, GNode *, const char *,
    size_t);

static void expand_pattern(const char *, size_t ,
	const char *, size_t, char**, char**);

static void
Targ_RemoveTmpTarg(void *, void *unused UNUSED);

bool
is_pattern(const GNode *gn)
{
	return strchr(gn->name, '%') != NULL;
}

/*
 * Called on each target of a dependency line: only targets of a rule
 * are patterns, a '%' prerequisite is not.
 */
void
may_register_as_pattern(GNode *gn)
{
	struct pattern *pat;
	const char *percent;
	size_t i;

	percent = strchr(gn->name, '%');
	if (percent == NULL)
		return;

	for (i = 0; i != npatterns; i++)
		if (patterns[i].gn == gn)
			return;

	if (npatterns == maxpatterns) {
		maxpatterns *= 2;
		patterns = ereallocarray(patterns, maxpatterns,
		    sizeof(*patterns));
	}
	pat = &patterns[npatterns++];
	pat->gn = gn;
	pat->prefix_len = percent - gn->name;
	pat->suffix = percent + 1;
	pat->suffix_len = strlen(pat->suffix);
}

void
Pattern_Init(void)
{
	maxpatterns = 32;
	patterns = ereallocarray(NULL, maxpatterns, sizeof(*patterns));
}

/*
 * Search for a file in hash table that matches the given pattern.
 *
 * It returns an allocated string with matched file name or NULL
 * if no match found.
 */
char *
find_file_hash_with_pattern(struct ohash *h, const char *pattern)
{
	unsigned int search;
	const char *entry;

	for (entry = ohash_first(h, &search); entry != NULL;
	    entry = ohash_next(h, &search)) {
		if (match_pattern(entry, pattern)) {
			if (DEBUG(PATTERN))
				printf("find_file_hash_with_pattern: "
				    "Matched file: %s\n",
				    entry);

			return estrdup(entry);
		}
	}
	return NULL;
}

/*-
 *-----------------------------------------------------------------------
 * expand_pattern --
 *	Expands the '%' wildcard in src by replacing it with pattern_value.
 *
 * Side Effects:
 *	Allocates memory for result which must be freed by caller.
 *	Sets *result to the expanded string and *eresult to its end.
 *-----------------------------------------------------------------------
 */
void
expand_pattern(const char *src, size_t src_len,
	const char *pattern_value, size_t pattern_value_len,
	char **result, char **eresult)
{
	size_t prefix_len;
	size_t suffix_len;
	size_t result_len;

	const char *percent = strchr(src, '%');
	if (!percent) {
		*result = strndup(src, src_len);
		if (*result == NULL)
			Fatal("expand_pattern: out of memory");
		*eresult = *result + src_len;
		return;
	}

	prefix_len = percent - src;
	suffix_len = src_len - prefix_len - 1;
	result_len = prefix_len + pattern_value_len + suffix_len;

	*result = emalloc(result_len + 1);
	memcpy(*result, src, prefix_len);
	memcpy(*result + prefix_len, pattern_value, pattern_value_len);
	memcpy(*result + prefix_len + pattern_value_len, percent + 1, suffix_len);
	(*result)[result_len] = '\0';
	*eresult = *result + result_len;
}

/*-
 * Targ_CopyCommands --
 *	Copy all commands from src GNode to dest GNode.
 *	Each command is duplicated in memory.
 */
static void
Targ_CopyCommands(GNode *dest, GNode *src)
{
	LstNode ln;
	struct command *cmd;
	const char *cmd_str;
	size_t elen;
	struct command *new_cmd;

	for (ln = Lst_First(&src->commands); ln != NULL; ln = Lst_Adv(ln)) {
		cmd = Lst_Datum(ln);
		cmd_str = cmd->string;
		elen = strlen(cmd_str);

		new_cmd = emalloc(offsetof(struct command, string) + elen + 1);
		new_cmd->location = cmd->location;
		memcpy(new_cmd->string, cmd_str, elen + 1);

		Lst_AtEnd(&dest->commands, new_cmd);
	}
}

/*-
 *-----------------------------------------------------------------------
 * Targ_CreateNodeFromPattern --
 *	Find the node named expanded_name, or create it if it does not
 *	exist yet. A created node remembers rule_gn, the pattern rule
 *	that required it. It is neither linked to any parent nor given
 *	children or commands: that happens when make examines it.
 *
 * Returns:
 *	The existing or newly created GNode.
 *-----------------------------------------------------------------------
 */
static GNode *
Targ_CreateNodeFromPattern(GNode *rule_gn, const char *expanded_name,
    const char *expanded_ename)
{
	GNode *new_gn;
	unsigned int slot;
	uint32_t hv;
	struct ohash *h;

	/* Search its place in ohash and insert it. */
	hv = ohash_interval(expanded_name, &expanded_ename);
	h = targets_hash();
	slot = ohash_lookup_interval(h, expanded_name, expanded_ename, hv);
	new_gn = ohash_find(h, slot);
	
	/* If the node already exists, return it. */
	if (new_gn != NULL)
	{
		if (DEBUG(PATTERN))
			printf("Node %.*s already exists, returning it\n",
			    (int)(expanded_ename - expanded_name), expanded_name);
		return new_gn;
	}

	if (DEBUG(PATTERN))
		printf("Creating node %.*s from %s\n",
		    (int)(expanded_ename - expanded_name), expanded_name,
		    rule_gn->name);
	
	/* Else: create a new node and insert it into the hash. */
	new_gn = Targ_NewGNi(expanded_name, expanded_ename);
	new_gn->expanded_from = rule_gn;
	ohash_insert(h, slot, new_gn);

	return new_gn;
}

/*-
 * Targ_BuildChildFromPatternParent --
 *	Expand a prerequisite of the pattern rule rule_gn with the given
 *	pattern value, then link the resulting node to parent_gn. Its own
 *	prerequisites are found when make examines it, like any other
 *	node without children.
 */
static void
Targ_BuildChildFromPatternParent(GNode *parent_gn, GNode *rule_gn,
    GNode *child, const char *pattern_value, size_t pattern_value_len)
{
	GNode *new_child;
	char *new_name;
	char *new_ename;

	/* Compute full name */
	expand_pattern(child->name,
	    strlen(child->name), pattern_value, pattern_value_len,
	    	&new_name, &new_ename);
	new_child = Targ_CreateNodeFromPattern(rule_gn, new_name, new_ename);
	free(new_name);

	/* Link new_child to parent_gn and vice-versa. */
	Lst_AtEnd(&parent_gn->children, new_child);
	Lst_AtEnd(&new_child->parents, parent_gn);

	parent_gn->children_left++;
}

/*-
 *-----------------------------------------------------------------------
 * Targ_BuildFromPattern --
 *	Give parent_gn the prerequisites and commands of the pattern
 *	rule pattern_gn, expanding each '%' with pattern_value.
 *-----------------------------------------------------------------------
 */
void
Targ_BuildFromPattern(GNode *parent_gn, GNode *pattern_gn,
    const char *pattern_value, size_t pattern_value_len)
{
	LstNode ln;
	GNode *child;

	/* Note: pattern_value_len can be 0 for empty stems (e.g., "lib%.a"
	 * matching "lib.a"), which is valid. */

	if (DEBUG(PATTERN))
		printf("Building children of %s from %s with stem %.*s\n",
		    parent_gn->name, pattern_gn->name,
		    (int)pattern_value_len, pattern_value);

	/* For each child of pattern_gn, create a new child for parent_gn
	 * replacing % with pattern_value. */
	for (ln = Lst_First(&pattern_gn->children); ln != NULL;
	    ln = Lst_Adv(ln)) {
		child = Lst_Datum(ln);
		Targ_BuildChildFromPatternParent(parent_gn, pattern_gn, child,
		    pattern_value, pattern_value_len);
	}

	/* Set impliedsrc to first child for $< variable support */
	if (!Lst_IsEmpty(&parent_gn->children)) {
		parent_gn->impliedsrc = Lst_Datum(
			Lst_First(&parent_gn->children));
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
 *	Example: "lib%.a" matches "libfoo.a".
 *
 * Returns:
 *	true if pattern matches name, false otherwise.
 *-----------------------------------------------------------------------
 */
bool
match_pattern(const char *name, const char *pattern)
{
	const char *percent;
	size_t prefix_len;
	size_t suffix_len;
	size_t name_len;

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

	return name_len >= prefix_len + suffix_len &&
	    memcmp(name, pattern, prefix_len) == 0 &&
	    memcmp(name + name_len - suffix_len, percent + 1, suffix_len) == 0;
}

/*-
 *-----------------------------------------------------------------------
 * Targ_FindPatternMatchingNode --
 *	Search the pattern rules for the one whose target matches the
 *	name of gnode_from with the shortest stem, as in GNU make (the
 *	first one wins a tie). gnode_from itself is skipped (a target
 *	like "file-%.in" matches its own pattern), and so is the rule
 *	that required it, so that a rule like "%: %.c" does not chain
 *	on itself.
 *
 * Returns:
 *	The matching GNode, or NULL if no match found.
 *	On a match, *stem and *stem_len are set to the part of the name
 *	that matched the '%' (*stem points into gnode_from->name, and
 *	*stem_len can be 0, e.g. "lib%.a" matching "lib.a").
 *-----------------------------------------------------------------------
 */
GNode *
Targ_FindPatternMatchingNode(const GNode *gnode_from, const char **stem,
    size_t *stem_len)
{
	const char *name = gnode_from->name;
	const struct pattern *pat;
	size_t name_len;
	size_t i;
	const struct pattern *best_pattern = NULL;
	size_t best_presuf_len = 0;

	/* If no pattern GNode exists we do not try to search one */
	if (npatterns == 0)
		return NULL;

	name_len = strlen(name);

	/* Iterate over each pattern, looking for a match */
	for (i = 0; i != npatterns; i++) {
		pat = &patterns[i];

		/* Prevent matching the node itself or its expanded form */
		if (pat->gn == gnode_from ||
		    pat->gn == gnode_from->expanded_from)
			continue;

		/* Check if the name is long enough to match the pattern. */
		if (name_len < pat->prefix_len + pat->suffix_len)
			continue;

		/* Skip a pattern that cannot give a shorter stem */
		if (best_pattern != NULL &&
		    pat->prefix_len + pat->suffix_len <= best_presuf_len)
			continue;

		/* Check if the prefix matches. */
		if (memcmp(name, pat->gn->name, pat->prefix_len) != 0)
			continue;
		if (memcmp(name + name_len - pat->suffix_len, pat->suffix,
		    pat->suffix_len) != 0)
			continue;

		/* Found a match, the best one so far (shortest stem) */
		*stem = name + pat->prefix_len;
		*stem_len = name_len - pat->prefix_len - pat->suffix_len;
		best_presuf_len = pat->prefix_len + pat->suffix_len;
		best_pattern = pat;

		/* An empty stem cannot be beaten */
		if (*stem_len == 0)
			break;
	}
	return best_pattern ? best_pattern->gn : NULL;
}

static bool
is_tmp_target(const GNode *gn)
{
	return gn->is_tmp || (gn->type & (OP_INTERMEDIATE|OP_SECONDARY));
}

/*-
 *-----------------------------------------------------------------------
 * Targ_SkipTmp --
 *	As in GNU make, a missing temporary target is not made unless a
 *	target that depends on it is out of date (see Targ_UnskipTmp).
 *	Meanwhile, it takes the modification time of its youngest child,
 *	so that its parents compare against its own prerequisites.
 *
 * Returns:
 *	true if gn is skipped, and must be deemed up-to-date.
 *-----------------------------------------------------------------------
 */
bool
Targ_SkipTmp(GNode *gn)
{
	if (!is_tmp_target(gn) || gn->tmp_needed || !is_out_of_date(gn->mtime))
		return false;

	if (DEBUG(PATTERN))
		printf("Skipping missing temporary target %s\n", gn->name);

	gn->tmp_skipped = true;
	gn->mtime = gn->youngest->mtime;
	return true;
}

/*-
 *-----------------------------------------------------------------------
 * Targ_UnskipTmp --
 *	A parent of gn is out of date: if gn was skipped, it must be made
 *	after all, and is examined again.
 *
 * Returns:
 *	true if gn was skipped.
 *-----------------------------------------------------------------------
 */
bool
Targ_UnskipTmp(GNode *gn)
{
	if (!gn->tmp_skipped)
		return false;

	if (DEBUG(PATTERN))
		printf("Temporary target %s needed after all\n", gn->name);

	gn->tmp_skipped = false;
	gn->tmp_needed = true;
	tmp_made = true;
	gn->built_status = UNKNOWN;
	return true;
}

static void
Targ_RemoveTmpTarg(void *child, void *unused UNUSED)
{
	GNode *gn = child;
	const char *file;

	/* As in GNU make, only a temporary target this run created is
	 * removed: one that existed already is kept, even if remade.
	 * A missing one is always skipped first, so tmp_needed tells it
	 * was created, by its own commands or by those of a target made
	 * by the same rule; one skipped or not made yet has no file. */
	if (!is_tmp_target(gn) || !gn->tmp_needed ||
	    !has_been_built(gn) || Targ_Precious(gn) ||
	    (gn->type & OP_SECONDARY)) {
		return;
	}

	if (DEBUG(PATTERN))
		printf("Targ_RemoveTmpTarg: Removing node %s\n", gn->name);

	file = gn->path != NULL ? gn->path : gn->name;
	if (eunlink(file) == 0) {
		if (!Targ_Silent(gn))
			printf("rm %s\n", file);
		gn->tmp_needed = false;
	} else {
		fprintf(stderr, "*** couldn't delete %s: %s\n", file,
		    strerror(errno));
	}
}

void
Targ_RemoveAllTmpTargets(void)
{
	GNode *gn;
	unsigned int i;
	struct ohash *h;

	/* Only a temporary target made after all is removed, and none
	 * if .SECONDARY is given */
	if (!tmp_made || allSecondary)
		return;

	if (DEBUG(PATTERN))
		printf("Removing all temporary targets...\n");

	h = targets_hash();
	for (gn = ohash_first(h, &i); gn != NULL; gn = ohash_next(h, &i)) {
		Lst_ForEach(&gn->children, Targ_RemoveTmpTarg, NULL);
	}
}

bool
expand_children_from_pattern(GNode *gn)
{
	GNode *matching;
	const char *stem;
	size_t stem_len;

	/* A target with commands already has its rule, so it does not need
	 * to be expanded. */
	if (!Lst_IsEmpty(&gn->commands))
		return false;

	if (gn->type & OP_PHONY)
		return false;

	if (DEBUG(PATTERN)) {
		printf("\t\t => No children found for \"%s\"\n", gn->name);
		printf("\t\t => Searching for matching pattern targets...\n");
	}

	matching = Targ_FindPatternMatchingNode(gn, &stem, &stem_len);
	if (matching != NULL) {
		if (DEBUG(PATTERN))
			printf("\t\t => Matching pattern target found: %s\n",
			    matching->name);

		/* Replace all % pattern of matching node with parent
		 * node and add it to the parent children list. */
		Targ_BuildFromPattern(gn, matching, stem, stem_len);

		/* A node created from a pattern and built by a pattern rule
		 * is an intermediate file. If the rule gives it nothing to
		 * do, then this is a final target, which must not be
		 * deleted. */
		gn->is_tmp = gn->expanded_from != NULL &&
		    (!Lst_IsEmpty(&gn->children) ||
		    !Lst_IsEmpty(&gn->commands));

		/* gn herits PRECIOUS from the pattern rule */
		gn->type |= matching->type & OP_PRECIOUS;

		if (DEBUG(PATTERN))
			printf("All children of %s have been built.\n\n",
			    gn->name);

		return true;
	}

	if (DEBUG(PATTERN))
		printf("\tNo pattern matching found for %s\n", gn->name);

	return false;
}
