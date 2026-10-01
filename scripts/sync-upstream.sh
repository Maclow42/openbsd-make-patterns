#!/bin/sh
# Sync this fork's make/ tree with the latest official OpenBSD make source:
# pull the OpenBSD source tree at $SRC_ROOT (fast-forward only), mirror
# its usr.bin/make onto the upstream branch, then merge those upstream
# changes into the current branch so local modifications (pattern rules,
# etc.) get combined with upstream updates via git's normal merge/conflict
# resolution.
#
# See scripts/lib.sh for $SRC_ROOT. Never switches you off your current
# branch (the upstream snapshot is built in a disposable worktree), so this
# is safe to run with work in progress on `main`.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
. "$ROOT/scripts/lib.sh"

if [ -n "$(git status --porcelain)" ]; then
    echo "error: working tree is not clean. Commit or stash your changes first:" >&2
    git status --short
    exit 1
fi

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"

require_src
echo "==> Pulling the OpenBSD source tree ($SRC_ROOT)..."
git -C "$SRC_ROOT" pull --ff-only --quiet

echo "==> Updating '$UPSTREAM_BRANCH' branch from official source ($SRC_MAKE)..."
if update_upstream_branch; then
    echo "==> Recorded new upstream snapshot."
else
    echo "==> Already up to date with upstream, nothing to merge."
    exit 0
fi

echo "==> Merging '$UPSTREAM_BRANCH' into '$CURRENT_BRANCH'..."
if git merge "$UPSTREAM_BRANCH" -m "Merge upstream OpenBSD make updates"; then
    echo "==> Merge successful, no conflicts."
else
    echo "==> Merge has conflicts. Resolve them, then: git add <files> && git commit"
    exit 1
fi
