# Shared helpers for scripts/*.sh. Must be sourced from the HOST (Debian),
# not inside the VM: it reads the official OpenBSD source tree checked out
# on the host. From the host this works transparently through the sshfs
# mount. Not executable on its own.

: "${SRC_ROOT:=/home/maclow/Documents/OpenBSD/src}"
SRC_MAKE="$SRC_ROOT/usr.bin/make"
UPSTREAM_BRANCH="upstream"
UPSTREAM_WORKTREE="/tmp/openbsd-make-upstream-wt"

require_src() {
  if [ ! -d "$SRC_MAKE" ]; then
    echo "error: $SRC_MAKE not found." >&2
    echo "       Run this script on the host (not inside the VM), or set \$SRC_ROOT." >&2
    exit 1
  fi
}

# Updates (creating if needed) the $UPSTREAM_BRANCH branch so its make/
# subtree exactly mirrors the current official OpenBSD make source, using a
# disposable worktree -- this never touches your currently checked-out
# branch. Exit status: 0 if a new upstream snapshot was committed, 1 if
# already up to date (not an error, just "nothing changed").
update_upstream_branch() {
  require_src
  git -C "$SRC_ROOT" pull --ff-only --quiet

  rm -rf "$UPSTREAM_WORKTREE"
  git worktree prune >/dev/null 2>&1 || true
  git config --global --add safe.directory "$UPSTREAM_WORKTREE"
  if git show-ref --verify --quiet "refs/heads/$UPSTREAM_BRANCH"; then
    git worktree add -q "$UPSTREAM_WORKTREE" "$UPSTREAM_BRANCH"
  else
    git worktree add -q --detach "$UPSTREAM_WORKTREE"
    (cd "$UPSTREAM_WORKTREE" && git checkout --orphan "$UPSTREAM_BRANCH" -q && git rm -rf -q . >/dev/null 2>&1 || true)
  fi

  find "$UPSTREAM_WORKTREE" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
  mkdir "$UPSTREAM_WORKTREE/make"
  cp -R "$SRC_MAKE"/. "$UPSTREAM_WORKTREE/make/"

  rc=0
  (
    cd "$UPSTREAM_WORKTREE"
    git add -A
    if git diff --cached --quiet; then
      exit 1
    fi
    git commit -q -m "Sync upstream make as of $(date +%Y-%m-%d)"
  ) || rc=$?

  git worktree remove --force "$UPSTREAM_WORKTREE" >/dev/null 2>&1 || true
  return $rc
}
