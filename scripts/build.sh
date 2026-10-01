#!/bin/sh
# Build OpenBSD make with pattern rule support.
#
# The built checkout is the one holding this script, or $PATTERNS_ROOT if
# set: `./openbsd.sh build` runs main's copy of this script on the current
# checkout that way, so it also works on branches without scripts/.
#
# Run this inside the VM (or via `./openbsd.sh build` from the host).
set -e
ROOT="${PATTERNS_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$ROOT/make"
make "$@"
