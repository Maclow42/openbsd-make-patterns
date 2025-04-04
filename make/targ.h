#ifndef TARG_H
#define TARG_H
/*	$OpenBSD: targ.h,v 1.16 2024/05/21 05:00:48 jsg Exp $ */

/*
 * Copyright (c) 2001 Marc Espie.
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

#include <sys/time.h>
/*
 * The TARG_ constants are used when calling the Targ_FindNode functions.
 * They simply tell the function what to do if the desired node is not found.
 * If the TARG_CREATE constant is given, a new, empty node will be created
 * for the target, placed in the table of all targets and its address returned.
 * If TARG_NOCREATE is given, a NULL pointer will be returned.
 */
#define TARG_CREATE	0x01	  /* create node if not found */
#define TARG_NOCREATE	0x00	  /* don't create it */

extern void Targ_Init(void);
extern GNode *Targ_NewGNi(const char *, const char *);
extern GNode * Targ_CopyGni(GNode *);
#define Targ_NewGN(n)	Targ_NewGNi(n, NULL);
extern GNode *Targ_FindNodei(const char *, const char *, int);
#define Targ_FindNode(n, i)	Targ_FindNodei(n, NULL, i)

extern bool match_pattern(const char *name, const char *pattern, char** expended);
extern GNode *Targ_FindPatternMatchingNode(const char *name, char** expended);



/* helper for constant nodes */
extern GNode *Targ_mk_special_node(const char *, size_t, uint32_t,
    unsigned int, unsigned char, unsigned int);

extern void Targ_FindList(Lst, Lst);
extern bool Targ_Ignore(GNode *);
extern bool Targ_Silent(GNode *);
extern bool Targ_Precious(GNode *);
extern void Targ_PrintCmd(void *);
extern void Targ_PrintType(int);
extern bool node_is_real(GNode *);

extern GNode *begin_node, *end_node, *interrupt_node, *DEFAULT;
struct ohash_info;

extern struct ohash_info gnode_info;

extern void Targ_setdirs(const char *, const char *);
extern const char *status_to_string(GNode *);
extern struct ohash *targets_hash(void);

extern bool match_pattern(const char *, const char *, char **);

#endif
