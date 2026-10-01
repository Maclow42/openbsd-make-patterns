#!/bin/sh
# Generate a `git diff`-formatted patch between this fork's make/ (including
# your uncommitted work in progress) and the official OpenBSD make source --
# i.e. real a/ b/ paths, blob hashes, rename detection, etc.
#
# upstream is the OpenBSD source tree at $SRC_ROOT as currently checked out:
# this script does not pull it, `sync-upstream.sh` does.
#
# See scripts/lib.sh for $SRC_ROOT. Never switches you off your current
# branch.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
. "$ROOT/scripts/lib.sh"

echo "==> Updating '$UPSTREAM_BRANCH' branch from official source ($SRC_MAKE)..." >&2
if update_upstream_branch; then
    echo "==> Recorded new upstream snapshot." >&2
else
    echo "==> Already up to date with upstream." >&2
fi

mkdir -p diffs
OUT="diffs/$(date +%Y%m%d-%H%M%S).diff"
git diff "$UPSTREAM_BRANCH" -- make/ > "$OUT"

echo "Diff written to $OUT" >&2
wc -l < "$OUT" | xargs echo "Lines:" >&2
