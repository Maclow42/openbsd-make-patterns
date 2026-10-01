#!/bin/sh
# Run the pattern-rules testsuite with a chosen make binary.
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
# Run this inside the VM (or via `./openbsd.sh test [...]` from the host).
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
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

echo "==> Testing with: $MAKE_BIN"
cd "$TREE/make/testsuite"
"$MAKE_BIN" test
