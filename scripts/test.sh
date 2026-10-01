#!/bin/sh
# Run the pattern-rules testsuite with a chosen make binary.
#
# Usage: scripts/test.sh [mine|system|gmake|custom[:/path/to/make]]
#   mine    (default) -- build then test with this fork's own make/make
#   system             -- test with the system `make` found in $PATH (bmake)
#   gmake               -- test with GNU make (`gmake`), e.g. as a reference
#                           for how real pattern rules should behave
#   custom[:/path]      -- test with a specific binary; defaults to
#                           $CUSTOM_MAKE or /usr/local/bin/make-patterns
#
# Recursive test runs pick up the same binary automatically: the testsuite's
# Makefiles use $(MAKE) internally, which both bmake and GNU make set to the
# binary that was actually invoked.
#
# Run this inside the VM (or via `./openbsd.sh test [...]` from the host).
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WHICH="${1:-mine}"

case "$WHICH" in
mine)
    sh "$ROOT/scripts/build.sh"
    MAKE_BIN="$ROOT/make/make"
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
    echo "usage: $0 [mine|system|gmake|custom[:/path/to/make]]" >&2
    exit 1
    ;;
esac

[ -x "$MAKE_BIN" ] || { echo "error: '$MAKE_BIN' not found or not executable" >&2; exit 1; }

echo "==> Testing with: $MAKE_BIN"
cd "$ROOT/make/testsuite"
"$MAKE_BIN" test
