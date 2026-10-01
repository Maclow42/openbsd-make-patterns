#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defines.h"
#include "cmd_exec.h"
#include "error.h"
#include "str.h"
#include "var.h"
#include "gnuvarfunc.h"

typedef bool (*gnu_command_handler)(const char *, const char *, SymTable *,
	bool, bool *, char **);

struct command {
	const char *name;
	size_t len;
	gnu_command_handler handler;
};

static const char *find_parenthesis_end(const char *);
static const char *skip_spaces(const char *);
static const struct command *find_command(const char *);
static bool command_shell(const char *, const char *, SymTable *, bool,
	bool *, char **);

static const struct command commands[] = {
	{ "shell", 5, command_shell },
	{ NULL, 0, NULL }
};

/*
 * Tries to find the end of a GNU variable function, starting at the open
 * parenthesis.
 * Returns NULL if the parentheses are unbalanced.
 */
static const char *
find_parenthesis_end(const char *p)
{
	int depth = 1;

	while (*p != '\0') {
		if (*p == '(')
			depth++;
		else if (*p == ')') {
			depth--;
			if (depth == 0)
				return p;
		}
		p++;
	}
	return NULL;
}

static const char *
skip_spaces(const char *p)
{
	while (ISSPACE(*p))
		p++;
	return p;
}

static const struct command *
find_command(const char *p)
{
	for (const struct command *cmd = commands; cmd->name != NULL; cmd++) {
		if (strncmp(p, cmd->name, cmd->len) != 0)
			continue;
		if (p[cmd->len] == ')' || ISSPACE(p[cmd->len]))
			return cmd;
	}
	return NULL;
}

static bool
command_shell(const char *arg, const char *end, SymTable *ctxt, bool err,
    bool *freePtr, char **result)
{
	char *cmd;
	char *expanded;
	char *output;
	char *exec_err;

	cmd = Str_dupi(arg, end);
	expanded = Var_Subst(cmd, ctxt, err);
	free(cmd);

	exec_err = NULL;
	output = Cmd_Exec(expanded, &exec_err);
	if (exec_err != NULL && DEBUG(VAR))
		printf(exec_err, expanded);
	free(expanded);

	*freePtr = true;
	*result = output;
	return true;
}

/*
 * Parses and executes a GNU variable function, returning true if the string
 * was recognized as a GNU variable function.
 * Result of the function is returned in *result, and should be freed by the
 * caller if *freePtr is set.
 * If the function is not recognized, returns false and leaves *result
 * unmodified.
 */
bool
GnuVar_ParseFunction(const char *str, SymTable *ctxt, bool err,
    size_t *lengthPtr, bool *freePtr, char **result)
{
	const char *start;
	const char *p;
	const char *arg;
	const char *end;
	const struct command *cmd;

	if (str[0] != '$' || str[1] != '(')
		return false;
	start = str;
	p = skip_spaces(str + 2);

	if ((cmd = find_command(p)) == NULL)
		return false;

	p += cmd->len;
	p = skip_spaces(p);
	arg = p;

	if ((end = find_parenthesis_end(start + 2)) == NULL) {
		Parse_Error(PARSE_FATAL,
		    "Unterminated GNU variable function in %s", start);
		*lengthPtr = strlen(str);
		*freePtr = false;
		*result = NULL;
		return true;
	}

	if (arg > end)
		arg = end;

	*lengthPtr = (end - start) + 1;
	return cmd->handler(arg, end, ctxt, err, freePtr, result);
}

bool
GnuVar_ParseFunctionSkip(const char *str, size_t *lengthPtr)
{
	const char *start;
	const struct command *cmd;
	const char *end;
	const char *p;

	if (str[0] != '$' || str[1] != '(')
		return false;
	start = str;

	p = skip_spaces(str + 2);
	cmd = find_command(p);
	if (cmd == NULL)
		return false;

	end = find_parenthesis_end(start + 2);
	if (end == NULL)
		return false;

	*lengthPtr = (end - start) + 1;
	return true;
}
