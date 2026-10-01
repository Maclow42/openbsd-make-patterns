#!/bin/sh
# Run the testsuite with a chosen make binary, then, for a BSD make, the
# OpenBSD regression tests of make(1), which check that the fork does not
# break anything OpenBSD relies on.
#
# Usage: scripts/test.sh [--branch REF] [mine|system|gmake|custom[:/path/to/make]]
#   mine    (default) -- build then test with this fork's own make/make
#   system             -- test with the system `make` found in $PATH (bmake)
#   gmake               -- test with GNU make (`gmake`), e.g. as a reference
#                           for how real pattern rules should behave
#   custom[:/path]      -- test with a specific binary; defaults to
#                           $CUSTOM_MAKE or /usr/local/bin/make-patterns
#
#   --branch REF        -- build and test REF (a branch, tag or commit)
#                           instead of the current checkout. REF is checked
#                           out in a disposable worktree, so the current
#                           checkout is left untouched and REF does not need
#                           to carry scripts/ (e.g. feature/* branches based
#                           on upstream). Only committed changes of REF are
#                           tested. With system/gmake/custom, that binary
#                           runs REF's testsuite.
#
# Recursive test runs pick up the same binary automatically: the testsuite's
# Makefiles use $(MAKE) internally, which both bmake and GNU make set to the
# binary that was actually invoked.
#
# The tested checkout is the one holding this script, or $PATTERNS_ROOT if
# set: that way, main's copy of this script also tests branches without
# scripts/.
#
# The regression tests are read from $REGRESS_SRC, by default
# $SRC_ROOT/regress/usr.bin/make, $SRC_ROOT defaulting to /usr/src. They
# should come from the source tree the upstream branch mirrors, to match
# the upstream version of make. They are skipped with gmake, as they test
# BSD make.
#
# Run this on OpenBSD.
set -e
ROOT="${PATTERNS_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
TREE="$ROOT"

if [ "$1" = "--branch" ]; then
    REF="$2"
    [ -n "$REF" ] || { echo "error: --branch needs a branch, tag or commit" >&2; exit 1; }
    shift 2
    git -C "$ROOT" rev-parse --verify -q "$REF^{commit}" >/dev/null ||
        { echo "error: unknown branch, tag or commit '$REF'" >&2; exit 1; }
    TREE="$(mktemp -d /tmp/make-test-wt.XXXXXX)"
    trap 'git -C "$ROOT" worktree remove --force "$TREE" >/dev/null 2>&1 || rm -rf "$TREE"; git -C "$ROOT" worktree prune' EXIT
    git -C "$ROOT" worktree add -q --detach "$TREE" "$REF"
    [ -d "$TREE/make/testsuite" ] ||
        { echo "error: '$REF' has no make/testsuite to run" >&2; exit 1; }
    echo "==> Testing $REF ($(git -C "$TREE" log -1 --format='%h %s'))"
fi

WHICH="${1:-mine}"

case "$WHICH" in
mine)
    (cd "$TREE/make" && make)
    MAKE_BIN="$TREE/make/make"
    ;;
system)
    MAKE_BIN="$(command -v make)" || { echo "error: no 'make' found in \$PATH" >&2; exit 1; }
    ;;
gmake)
    MAKE_BIN="$(command -v gmake)" || { echo "error: 'gmake' not found (try: pkg_add gmake)" >&2; exit 1; }
    ;;
custom:*)
    MAKE_BIN="${WHICH#custom:}"
    ;;
custom)
    MAKE_BIN="${CUSTOM_MAKE:-/usr/local/bin/make-patterns}"
    ;;
*)
    echo "usage: $0 [--branch REF] [mine|system|gmake|custom[:/path/to/make]]" >&2
    exit 1
    ;;
esac

[ -x "$MAKE_BIN" ] || { echo "error: '$MAKE_BIN' not found or not executable" >&2; exit 1; }
# Tests run $(MAKE) from their own directory: BSD make keeps a relative
# path as is, so make it absolute.
MAKE_BIN="$(cd "$(dirname "$MAKE_BIN")" && pwd)/$(basename "$MAKE_BIN")"

# Runs the OpenBSD regression tests of make(1) with $MAKE_BIN, in a copy of
# them since they write next to their Makefile.
run_regress() {
    src="${REGRESS_SRC:-${SRC_ROOT:-/usr/src}/regress/usr.bin/make}"
    if [ ! -f "$src/Makefile" ]; then
        echo "==> Regression tests not found in $src, skipped"
        return 0
    fi
    work="$(mktemp -d /tmp/make-regress.XXXXXX)"
    cp -R "$src"/. "$work"
    echo "==> Running OpenBSD regression tests ($src)"
    (cd "$work" && "$MAKE_BIN" REGRESS_FAIL_EARLY=no \
        REGRESS_LOG="$work/.results" regress > "$work/.output" 2>&1) || true
    if [ ! -s "$work/.results" ]; then
        echo "error: the regression tests did not run:"
        tail -5 "$work/.output"
        rm -rf "$work"
        return 1
    fi
    grep -v '^SUCCESS' "$work/.results" | sed "s|$work/||"
    echo "Passed:          $(grep -c '^SUCCESS' "$work/.results")"
    echo "Expected fails:  $(grep -c '^XFAIL' "$work/.results")"
    failed="$(grep -cv -e '^SUCCESS' -e '^XFAIL' "$work/.results")" || true
    echo "Failed:          $failed"
    rm -rf "$work"
    [ "$failed" -eq 0 ]
}

status=0
echo "==> Testing with: $MAKE_BIN"
(cd "$TREE/make/testsuite" && "$MAKE_BIN" test) || status=1
if [ "$WHICH" != gmake ]; then
    run_regress || status=1
fi
exit $status
