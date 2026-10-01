# OpenBSD Make with Pattern Rules Support

This project extends OpenBSD's make utility to support GNU Make-style pattern rules.

## What are Pattern Rules?

Pattern rules define how to build targets based on filename patterns using the `%` wildcard character. Unlike suffix rules, pattern rules:

- Use explicit patterns (e.g., `%.o: %.c`) instead of implicit suffix lists
- Support patterns anywhere in the filename, not just at the end
- Allow multiple wildcards and complex transformations
- Provide clearer and more maintainable Makefiles
- Are the standard in GNU Make and more widely understood

### Advantages over Suffix Rules

**Suffix Rules (traditional):**
```makefile
.SUFFIXES: .c .o
.c.o:
	cc -c $<
```

**Pattern Rules (modern):**
```makefile
%.o: %.c
	cc -c $< -o $@
```

Pattern rules offer:
- Better readability: the relationship between source and target is explicit
- More flexibility: patterns can match any part of the filename
- Automatic variables: `$@` (target), `$<` (first prerequisite), `$^` (all prerequisites)
- Compatibility: widely used in modern build systems

## Examples

### Basic Pattern Rule
```makefile
%.o: %.c
	gcc -c $< -o $@
```
Builds `foo.o` from `foo.c`, `bar.o` from `bar.c`, etc.

### Multiple Prerequisites
```makefile
%.o: %.c %.h
	gcc -c $< -o $@
```

### Pattern in Subdirectories
```makefile
build/%.o: src/%.c
	gcc -c $< -o $@
```

### Multiple Extensions
```makefile
%.pdf: %.tex
	pdflatex $<

%.html: %.md
	markdown $< > $@
```

## Project Modifications

This implementation adds pattern rule support to OpenBSD make through the following key modifications:

### Core Features

1. **Pattern Detection and Matching** (`targ.c`, `targ.h`)
   - `match_pattern()`: Matches filenames against patterns with `%` wildcards
   - `Targ_FindPatternMatchingNode()`: Searches for pattern rules matching a target
   - `Targ_BuildFromPattern()`: Expands pattern rules into concrete targets
   - Pattern nodes tracking via `is_pattern` flag in GNode structure

2. **Dynamic Target Creation** (`targ.c`)
   - `Targ_CreateNodeFromPattern()`: Creates new targets from pattern templates
   - Pattern expansion: replaces `%` with matched stem
   - Command copying: transfers recipes from pattern to concrete targets
   - Temporary target management: `is_tmp` flag for intermediate files
   - New GNode fields (`gnode.h`):
     - `is_pattern`: Indicates if the node represents a pattern rule
     - `expanded_from`: Points to the original pattern node
     - `is_tmp`: Marks temporary targets for cleanup

3. **Children Expansion** (`expandchildren.c`)
   - Modified `expand_children_from()` to search for pattern matches
   - Automatic prerequisite generation from pattern rules
   - Fallback to pattern rules when no explicit dependencies exist

4. **Directory Search** (`dir.c`)
   - `find_file_hashi_with_pattern()`: Pattern-aware file lookup
   - Integration with existing directory caching mechanism

5. **Debug Support** (`defines.h`, `main.c`)
   - New `DEBUG_PATTERN` flag (0x100000)
   - Activated with `-dP` command-line option
   - Detailed pattern matching trace output

6. **Cleanup** (`engine.c`)
   - `Targ_RemoveAllTmpChildren()`: Removes intermediate files after build
   - Automatic cleanup of temporary pattern-generated targets

## Development environment

This fork is developed inside an OpenBSD QEMU VM (the real toolchain and
`make.1`/`regress`-style testing need real OpenBSD), bridged to the host via
sshfs so this checkout can be edited with normal editors on the host while
building/testing happens on OpenBSD. See `../vms/openbsd.sh` on the host for
the VM lifecycle (`start`, `stop`, `ssh`, `status`, ...).

## Building

```sh
./scripts/build.sh
```

Equivalent to `cd make && make`. From the host (VM running): `../openbsd.sh build`.

This produces the `make` binary with pattern support enabled.

## Testing

The test suite lives in `make/testsuite/`, one directory per case, driven by
a top-level `Makefile`.

### Run All Tests
```sh
./scripts/test.sh              # builds and tests with this fork's own make
./scripts/test.sh system       # tests with the system make (bmake)
./scripts/test.sh gmake        # tests with GNU make, as a behavior reference
./scripts/test.sh custom:/path/to/make
```
From the host (VM running): `../openbsd.sh test [--branch REF] [mine|system|gmake|custom[:/path]]`.

### Test Another Branch
```sh
./scripts/test.sh --branch feature/pattern-rules
./scripts/test.sh --branch integration/all-features gmake
```
`--branch` builds and tests a branch, tag or commit in a disposable worktree:
the current checkout is left untouched, and the tested branch does not need
`scripts/` (the `feature/*` branches start from `upstream` and don't have
it). Only committed changes are tested.

A test passes only if its `make test` exits with status 0 and prints `[OK]`;
the runner exits with an error if any test fails.

Recursive test runs automatically use whichever binary you picked -- the
testsuite's Makefiles call `$(MAKE)` internally, which both bmake and GNU
make set to the binary that was actually invoked.

### Run Individual Test
```sh
cd make/testsuite/01-basic-pattern
make clean all
make test
```

### Test Categories

- **01-04**: Basic pattern matching
- **05-09**: Multiple targets and rules
- **10-12**: Special operators and directory patterns
- **13-14**: Pattern priority and automatic variables
- **15-18**: Edge cases (empty stems, subdirectories)
- **19-23**: Target types (secondary, intermediate, VPATH)
- **24-26**: Advanced features (static patterns, GNU shell function)

### Debug Mode

Enable verbose pattern matching output:
```sh
make/make -dP
```

## Keeping up with upstream OpenBSD make

Run these **from the host** (they read the official OpenBSD source tree
checked out there, and write through the sshfs mount):

```sh
./scripts/sync-upstream.sh   # or: ../openbsd.sh sync
```
Mirrors the current official `usr.bin/make` onto an `upstream` branch and
merges it into your current branch, so upstream changes and local pattern
rule modifications combine via git's normal merge/conflict resolution.

```sh
./scripts/diff-upstream.sh   # or: ../openbsd.sh diff
```
Writes a timestamped, git-formatted patch (`diff --git a/... b/...`, with
blob hashes and rename detection) of this fork's `make/` -- including
uncommitted work in progress -- against the official source, to `diffs/`,
for review or for sending a patch upstream.

Both commands maintain an `upstream` branch mirroring the official source,
built in a disposable `git worktree` -- they never check out or touch your
current branch.

## Compatibility

This implementation maintains backward compatibility with OpenBSD make while adding GNU Make pattern rule semantics. Traditional suffix rules continue to work as before.

## License

This project is based on OpenBSD make, which is distributed under the BSD license. See individual source files for detailed copyright information.
