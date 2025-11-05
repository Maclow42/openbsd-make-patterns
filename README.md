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

## Building

The project uses OpenBSD's standard make build system:

```sh
cd make
make
```

This produces the `make` binary with pattern support enabled.

## Testing

The project includes a comprehensive test suite in `testsuite_patterns/`:

### Run All Tests
```sh
cd testsuite_patterns
./launch_testsuite.sh
```

### Run Individual Test
```sh
cd testsuite_patterns/01-basic-pattern
../../make/make
```

### Test Categories

- **01-04**: Basic pattern matching
- **05-09**: Multiple targets and rules
- **10-12**: Special operators and directory patterns
- **13-14**: Pattern priority and automatic variables
- **15-18**: Edge cases (empty stems, subdirectories)
- **19-23**: Target types (secondary, intermediate, VPATH)
- **24-32**: Advanced features (static patterns, nested patterns, terminal rules)

### Debug Mode

Enable verbose pattern matching output:
```sh
../../make/make -dP
```

## Compatibility

This implementation maintains backward compatibility with OpenBSD make while adding GNU Make pattern rule semantics. Traditional suffix rules continue to work as before.

## License

This project is based on OpenBSD make, which is distributed under the BSD license. See individual source files for detailed copyright information.
