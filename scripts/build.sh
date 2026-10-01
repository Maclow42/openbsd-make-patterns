#!/bin/sh
# Build OpenBSD make with pattern rule support.
# Run this inside the VM (or via `./openbsd.sh build` from the host).
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT/make"
make "$@"
