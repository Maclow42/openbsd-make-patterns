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

`$^` expands to all the prerequisites of the target:

```makefile
prog: a.o b.o
	cc -o $@ $^
```

### `$(shell ...)`

`$(shell command)` expands to the output of `command`, and nests with
variables and other `$(shell ...)` calls:

```makefile
FILES = $(shell ls)
```

### Debugging

`make -dP` traces how targets are matched against pattern rules.

## Status

The testsuite follows GNU make's behavior: GNU make passes all 28 tests,
this fork passes 24. Known differences with GNU make:

- double-colon pattern rules (test 10)
- several pattern rules for the same target with different prerequisites,
  e.g. `%.o: %.c` and `%.o: %.cpp`: GNU make keeps both and picks the one
  whose prerequisites exist, this fork merges them (test 15)
- `.INTERMEDIATE` on a file built by a pattern rule (test 17)
- pattern-specific variables, e.g. `%.out: FLAGS = ok` (test 24)
- static pattern rules (`a.o b.o: %.o: %.c`) are not supported
- only one `%` per pattern
- `.PHONY` targets are matched against pattern rules, which GNU make never
  does (a catch-all `%::` rule also builds `clean`)

Redefining the commands of an ordinary target keeps the BSD behavior: the
first definition wins and the next ones are ignored.

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

## Development environment

This fork is developed inside an OpenBSD QEMU VM, as building and testing
need real OpenBSD. The VM is bridged to the host via sshfs, so this
checkout can be edited with normal editors on the host. `../openbsd.sh`, on
the host, manages the VM (`start`, `stop`, `status`, `ssh`) and wraps the
scripts below: `build` and `test` run inside the VM over SSH, `sync` and
`diff` run on the host.

`../openbsd.sh build` and `../openbsd.sh test` always run main's copy of
the scripts on the current checkout, so they work on every branch,
including the `feature/*` branches, which have no `scripts/`.

## Building

```sh
../openbsd.sh build      # from the host
```

This produces `make/make`. Inside the VM, `cd make && make` does the same.

## Testing

The testsuite lives in `make/testsuite/`, one directory per test, run by
`make/testsuite/Makefile`. Each feature branch carries the tests of its
feature.

```sh
../openbsd.sh test                 # build and test the current checkout
../openbsd.sh test gmake           # same tests with GNU make
../openbsd.sh test system          # same tests with the system make
../openbsd.sh test custom:/path/to/make
../openbsd.sh test --branch feature/pattern-rules
../openbsd.sh test --branch feature/pattern-rules gmake
```

Without `--branch`, the current checkout is tested as it is on disk,
uncommitted changes included. `--branch REF` builds and tests a branch, tag
or commit in a disposable worktree instead, leaving the checkout untouched;
only committed changes are tested. Inside the VM, `./scripts/test.sh` takes
the same arguments, on branches that have `scripts/`.

A test passes only if its `make test` exits with status 0 and prints
`[OK]`. The run fails if any test fails. GNU make is the reference: a test
that fails with `gmake` is a wrong test.

To run a single test, inside the VM:

```sh
cd make/testsuite/01-basic-pattern
../../make clean all
../../make test
```

### Adding a test

Create `make/testsuite/NN-name/Makefile` on the branch of the feature it
tests, with `all`, `test` (prints `[OK]` or `[KO]` and exits accordingly)
and `clean` targets, following the existing tests. Everything a test writes
in its directory is ignored by git, except its `Makefile`: input files a
test needs must be listed in `.gitignore`, like `14-patterned-file/file.in`.
Check that the test passes with `gmake`.

### Tests

Pattern rules (`feature/pattern-rules`):
- **01-04**: Basic pattern rules (no prerequisite, explicit prerequisite,
  explicit rule taking precedence, one rule per extension)
- **05-09**: Independent and chained rules, one rule building several
  targets, removal of intermediate files
- **10-11**: Double-colon (`::`) pattern rules
- **12-15**: `%` in a directory, redefined pattern rules (last one wins),
  searching a file matching a pattern, choosing a rule by existing
  prerequisites
- **16-18**: `.SECONDARY`, `.INTERMEDIATE` and `.PRECIOUS` targets
- **19-22**: `VPATH`, double extensions, a pattern rule over a list of
  targets, `%` in the middle of a name
- **23-25**: Substitution references, pattern-specific variables, removal
  order of intermediate files

`$(shell ...)` (`feature/gnu-shell-func`):
- **26-27**: Basic, nested and tricky `$(shell ...)` expansions

`$^` (`feature/automatic-vars`):
- **28**: `$^` (all prerequisites)

## Development workflow

### Branches

| Branch | Content | Changed by |
|---|---|---|
| `upstream` | Mirror of the official OpenBSD make | `../openbsd.sh sync` only |
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
../openbsd.sh test                 # uncommitted work included
../openbsd.sh test gmake           # the tests themselves must pass here
git commit
```

### Integrating

```sh
git checkout integration/all-features
git merge --no-ff feature/pattern-rules
../openbsd.sh test
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

`../openbsd.sh sync` (or `./scripts/sync-upstream.sh`, from the host, on a
branch that has `scripts/`) mirrors
the official `usr.bin/make`, read from the OpenBSD source tree checked out on
the host, onto the `upstream` branch, then merges it into the current
branch. Run it on the integration branch, then bring the update to every
feature:

```sh
git checkout integration/all-features
../openbsd.sh sync
git checkout feature/pattern-rules
git merge upstream                 # same for each feature/* branch
../openbsd.sh test
```

then integrate as usual. The `upstream` branch is built in a disposable
worktree and never edited by hand.

### Sending a feature upstream

```sh
git diff upstream feature/pattern-rules -- make/ ':!make/testsuite'
```

gives the patch of one feature alone. `../openbsd.sh diff` (or
`./scripts/diff-upstream.sh`, from the host, on a branch that has
`scripts/`) refreshes `upstream`, then writes the diff of the current
checkout's `make/` against it, uncommitted changes included, to `diffs/`.

## License

Based on OpenBSD make, distributed under the BSD license. See the source
files for copyright details.
