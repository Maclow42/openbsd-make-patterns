# OpenBSD make with GNU make extensions

A fork of OpenBSD's `make(1)` adding three GNU make features, each developed
on its own branch so it can be reviewed and sent upstream on its own:

| Feature | Branch |
|---|---|
| Pattern rules (`%.o: %.c`) | `feature/pattern-rules` |
| `$^` automatic variable | `feature/automatic-vars` |
| `$(shell ...)` function | `feature/gnu-shell-func` |

`main` combines the three features with the development tooling.

## Features

### Pattern rules

A pattern rule uses `%` to describe how to build a whole family of targets.
The part of the name matched by `%`, the stem, is substituted in the
prerequisites:

```makefile
%.o: %.c
	cc -c $< -o $@
```

builds `foo.o` from `foo.c`, `bar.o` from `bar.c`, and so on. Compared to
suffix rules (`.c.o:`), the relationship between target and source is
explicit, and `%` is not limited to suffixes:

```makefile
%.o: %.c %.h            # several prerequisites
	cc -c $< -o $@

build/%.o: src/%.c      # directories
	cc -c $< -o $@

lib%.a: lib%.src        # prefix and suffix
	cp $< $@
```

Rules chain, and intermediate files built along the way are removed at the
end of the build, as in GNU make:

```makefile
%.html: %.tmp
	cp $< $@
%.tmp: %.md
	cp $< $@
```

`make doc.html` builds `doc.tmp` from `doc.md`, then `doc.html`, then
removes `doc.tmp`. `.SECONDARY` and `.PRECIOUS` keep such files. Defining
the same pattern rule twice replaces the first definition (the last one
wins), and `$<` refers to the first prerequisite of the matched rule.

### `$^`

`$^` expands to all the prerequisites of the target, in order and without
duplicates:

```makefile
prog: a.o b.o
	cc -o $@ $^
```

### `$(shell ...)`

`$(shell command)` expands to the output of `command`, and nests with
variables and other `$(shell ...)` calls. A failing command gives a
warning, as with `!=`:

```makefile
FILES = $(shell ls)
```

### Debugging

`make -dP` traces how targets are matched against pattern rules.

## Status

The testsuite follows GNU make's behavior: GNU make passes all 40 tests.
This fork passes 29 and fails 11, each one a known difference with GNU
make, marked as such in the testsuite:

- a target without prerequisites but with its own commands also runs the
  commands of a matching pattern rule (test 31)
- `$*` keeps its BSD meaning, the target name minus a suffix listed in
  `.SUFFIXES`, instead of being the stem (test 39); use `$<`
- the commands building an intermediate file of a chain run twice
  (test 40), and a second run rebuilds the chain once its intermediate
  file is removed (test 29)
- when several pattern rules match, the first one wins instead of the one
  with the shortest stem (test 30)
- several pattern rules for the same target with different prerequisites,
  e.g. `%.o: %.c` and `%.o: %.cpp`, are merged: GNU make keeps both and
  picks the one whose prerequisites exist (test 15)
- `.PHONY` targets are matched against pattern rules, which GNU make never
  does (test 32)
- a redefined double-colon pattern rule keeps its first definition
  (test 10)
- `.INTERMEDIATE` is not honored (test 17)
- pattern-specific variables, e.g. `%.out: FLAGS = ok` (test 24), and
  static pattern rules, e.g. `a.o b.o: %.o: %.c` (test 33), are not
  supported

Of the GNU make functions, only `$(shell ...)` is supported: `patsubst`,
`wildcard` and the others are not. Substitution references such as
`$(SRCS:%.c=%.o)` work, as in any BSD make. Redefining the commands of an
ordinary target keeps the BSD behavior: the first definition wins and the
next ones are ignored.

The fork passes the OpenBSD regression tests of make(1) like the upstream
make it is based on.

## Implementation

Pattern rules (`feature/pattern-rules`):
- `patterns.c`, `patterns.h`: every node whose name contains `%` is
  registered as a pattern (`may_register_as_pattern()`, called from
  `Targ_mk_node()` in `targ.c`). `Targ_FindPatternMatchingNode()` and
  `match_pattern()` find the rule matching a target, and
  `Targ_BuildFromPattern()` instantiates it, creating the prerequisites as
  temporary nodes.
- `expandchildren.c`: `expand_children_from()` falls back to pattern rules
  for a target without prerequisites.
- `dir.c`: `find_file_hashi()` resolves names containing `%` against the
  directory cache.
- `gnode.h`: `expanded_from` (the pattern a node comes from) and `is_tmp`
  (intermediate file to remove).
- `main.c`, `job.c`: `Targ_RemoveAllTmpTargets()` removes intermediate files
  at the end of the build and when a job fails.
- `parse.c`: a redefined pattern rule replaces the previous commands.
- `var.c`: `$<` is allowed in commands expanded from a pattern.
- `main.c`, `defines.h`: the `-dP` debug flag (`DEBUG_PATTERN`).

`$^` (`feature/automatic-vars`): a new dynamic variable `MODIFIEDSRC`
(`var_int.h`, `var.h`, `var.c`, `generate.c`, `symtable.h`), filled in
`Make_DoAllVar()` in `engine.c`.

`$(shell ...)` (`feature/gnu-shell-func`): `gnuvarfunc.c` and
`gnuvarfunc.h` parse GNU make style functions (balanced parentheses,
nested expansions) and run `shell`. `var.c` hooks them into `Var_Parse()`,
`Var_ParseSkip()` and `Var_Check_for_target()`.

## Requirements

Building and testing need OpenBSD. `scripts/sync-upstream.sh` and
`scripts/diff-upstream.sh` need a git checkout of the OpenBSD source tree
(e.g. a clone of https://github.com/openbsd/src) at `$SRC_ROOT`, by default
`/usr/src`; they only use git and a POSIX shell, so they also run on
another system holding that tree.

The `feature/*` branches have no `scripts/`: run main's copy on them, the
scripts take the checkout to work on from `$PATTERNS_ROOT`:

```sh
git show main:scripts/test.sh > /tmp/test.sh
PATTERNS_ROOT=$PWD sh /tmp/test.sh [arguments]
```

## Building

```sh
./scripts/build.sh       # or: cd make && make
```

This produces `make/make`.

## Testing

The testsuite lives in `make/testsuite/`, one directory per test, run by
`make/testsuite/Makefile`. Each feature branch carries the tests of its
feature.

```sh
./scripts/test.sh                  # build and test the current checkout
./scripts/test.sh gmake            # same tests with GNU make
./scripts/test.sh system           # same tests with the system make
./scripts/test.sh custom:/path/to/make
./scripts/test.sh --branch feature/pattern-rules
./scripts/test.sh --branch feature/pattern-rules gmake
```

Without `--branch`, the current checkout is tested as it is on disk,
uncommitted changes included. `--branch REF` builds and tests a branch, tag
or commit in a disposable worktree instead, leaving the checkout untouched;
only committed changes are tested.

A test passes only if its `make test` exits with status 0 and prints
`[OK]`. GNU make is the reference: every test must pass with `gmake`, and
a test failing with `gmake` is a wrong test.

A test whose directory holds a `KNOWN_FAILURE` file is a known difference
between this fork and GNU make, explained in that file. With this fork, it
is reported as `[XFAIL]` when it fails, which does not fail the run, and as
`[XPASS]` when it passes, which does: the bug got fixed, so remove the
file. `KNOWN_FAILURE` files are ignored with `gmake`. A run thus fails on
a regression (`[KO]`) or an unexpected success (`[XPASS]`).

Except with `gmake`, the run goes on with the OpenBSD regression tests of
make(1) (`regress/usr.bin/make`), which check that the fork does not break
anything OpenBSD relies on. They are read from `$REGRESS_SRC`, by default
`$SRC_ROOT/regress/usr.bin/make`: use the source tree the `upstream` branch
mirrors, so they match the upstream version of make. The
OpenBSD `Makefile` itself lists the tests expected to fail (`XFAIL`); any
other failure fails the run.

To run a single test:

```sh
cd make/testsuite/01-basic-pattern
../../make clean all
../../make test
```

### Adding a test

Create `make/testsuite/NN-name/Makefile` on the branch of the feature it
tests, following the existing tests:

- `all` builds, `test` prints `[OK]` or `[KO]` and exits with status 0 or
  1, `clean` removes everything but the `Makefile` and `KNOWN_FAILURE`
  (``rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE` ``)
- one behavior per test, checked through what the commands produce: use
  `$@` and `$<` rather than file names written by hand, so that the test
  also checks which file the rule was applied to
- a test calling `$(MAKE)` again, e.g. to run make twice, runs it from its
  own directory

Everything a test writes in its directory is ignored by git, except its
`Makefile` and `KNOWN_FAILURE`: input files a test needs must be listed in
`.gitignore`, like `14-patterned-file/file.in`.

Check that the test passes with `gmake`. If it fails with this fork, add a
`KNOWN_FAILURE` file whose first line says why.

### Tests

Pattern rules (`feature/pattern-rules`):
- **01-04**: Basic pattern rules (no prerequisite, explicit prerequisite,
  explicit rule taking precedence, only the first `%` of a prerequisite
  replaced)
- **05-09**: Independent and chained rules, one rule building several
  targets in one run, removal of intermediate files
- **10-11**: Double-colon (`::`) pattern rules
- **12-15**: `%` in a directory, redefined pattern rules (last one wins),
  searching a file matching a pattern, choosing a rule by existing
  prerequisites
- **16-18**: `.SECONDARY`, `.INTERMEDIATE` and `.PRECIOUS` targets
- **19-22**: `VPATH`, double extensions, a pattern rule over a list of
  targets, `%` in the middle of a name
- **23-25**: Substitution references, pattern-specific variables, removal
  order of intermediate files
- **29-33**: Second run doing nothing, shortest stem, explicit rule without
  prerequisites, `.PHONY` targets, static pattern rules
- **34-37**: `$@` and `$<`, targets in a directory, parallel build (`-j`),
  suffix rules alongside pattern rules
- **39-40**: `$*` (the stem), intermediate file built once

`$(shell ...)` (`feature/gnu-shell-func`):
- **26-27**: Basic, nested and tricky `$(shell ...)` expansions
- **38**: `$(shell ...)` in an immediate assignment and a target list,
  output on several lines, empty output, failing command, a variable
  named like a function

`$^` (`feature/automatic-vars`):
- **28**: `$^` (all prerequisites, in order, without duplicates)

## Development workflow

### Branches

| Branch | Content | Changed by |
|---|---|---|
| `upstream` | Mirror of the official OpenBSD make | `scripts/sync-upstream.sh` only |
| `feature/*` | One feature and its tests, based on `upstream` | Development |
| `tooling/test-branch` | Scripts, README, `.gitignore` | Tooling changes |
| `integration/all-features` | Everything merged together, to validate | Integration |
| `main` | Released state | Fast-forward from integration only |

Never commit feature code directly on `main`: each feature must stay a
clean patch against `upstream`.

### Working on a feature

```sh
git checkout feature/pattern-rules
# edit, add or update tests
git show main:scripts/test.sh > /tmp/test.sh
PATTERNS_ROOT=$PWD sh /tmp/test.sh        # uncommitted work included
PATTERNS_ROOT=$PWD sh /tmp/test.sh gmake  # the tests must pass here too
git commit
```

### Integrating

```sh
git checkout integration/all-features
git merge --no-ff feature/pattern-rules
./scripts/test.sh
git checkout main
git merge --ff-only integration/all-features
git push origin main integration/all-features feature/pattern-rules
```

When a test passes on its feature branch but fails on the integration
branch, two features interact: fix it before moving `main`. If
`--ff-only` refuses, `main` got a commit of its own: merge `main` into the
integration branch first.

Tooling changes follow the same path from `tooling/test-branch`.

### Creating a feature

```sh
git checkout -b feature/new-feature upstream
git checkout main -- .gitignore
git commit -m "[ADD] .gitignore for build products and testsuite outputs"
```

Always start from `upstream`, never from `main`.

### Keeping up with upstream OpenBSD make

`./scripts/sync-upstream.sh`, on a branch that has `scripts/`, pulls the
OpenBSD source tree at `$SRC_ROOT`, mirrors its `usr.bin/make` onto the
`upstream` branch, then merges it into the current branch. Run it on the
integration branch, then bring the update to every feature:

```sh
git checkout integration/all-features
./scripts/sync-upstream.sh
git checkout feature/pattern-rules
git merge upstream                 # same for each feature/* branch
PATTERNS_ROOT=$PWD sh /tmp/test.sh # main's copy, see Requirements
```

then integrate as usual. The `upstream` branch is built in a disposable
worktree and never edited by hand.

### Sending a feature upstream

```sh
git diff upstream feature/pattern-rules -- make/ ':!make/testsuite'
```

gives the patch of one feature alone. `./scripts/diff-upstream.sh`, on a
branch that has `scripts/`, refreshes `upstream` from the source tree as it is checked out,
without pulling it, then writes the diff of the current checkout's `make/`
against it, uncommitted changes included, to `diffs/`.

## License

Based on OpenBSD make, distributed under the BSD license. See the source
files for copyright details.
