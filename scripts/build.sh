#!/bin/sh
# Build OpenBSD make with pattern rule support.
#
# The built checkout is the one holding this script, or $PATTERNS_ROOT if
# set: that way, main's copy of this script also builds branches without
# scripts/.
#
# Run this on OpenBSD.
set -e
ROOT="${PATTERNS_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$ROOT/make"
make "$@"
