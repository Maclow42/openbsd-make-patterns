# Shared helpers for scripts/*.sh. Not executable on its own; the sourcing
# script sets $ROOT.
#
# They read a git checkout of the official OpenBSD source tree, at
# $SRC_ROOT (default: /usr/src). Only git and a POSIX shell are needed, so
# they work on any system holding that tree, OpenBSD or not.

: "${SRC_ROOT:=/usr/src}"
SRC_MAKE="$SRC_ROOT/usr.bin/make"
UPSTREAM_BRANCH="upstream"
UPSTREAM_WORKTREE="/tmp/openbsd-make-upstream-wt"

require_src() {
  if [ ! -d "$SRC_MAKE" ]; then
    echo "error: $SRC_MAKE not found." >&2
    echo "       Set \$SRC_ROOT to a checkout of the OpenBSD source tree." >&2
    exit 1
  fi
}

# The checkout may belong to another user than the worktree (e.g. on a
# network mount), which git refuses: trust the worktree for this command
# only rather than adding it to the global git configuration.
wt_git() {
  git -C "$UPSTREAM_WORKTREE" -c safe.directory="$UPSTREAM_WORKTREE" "$@"
}

# Updates (creating if needed) the $UPSTREAM_BRANCH branch so its make/
# subtree exactly mirrors $SRC_MAKE as currently checked out, using a
# disposable worktree -- this never touches your currently checked-out
# branch, nor the source tree. Exit status: 0 if a new upstream snapshot
# was committed, 1 if already up to date (not an error, just "nothing
# changed"). The commit message names the OpenBSD source commit mirrored.
update_upstream_branch() {
  require_src

  rm -rf "$UPSTREAM_WORKTREE"
  git worktree prune >/dev/null 2>&1 || true
  if git show-ref --verify --quiet "refs/heads/$UPSTREAM_BRANCH"; then
    git worktree add -q "$UPSTREAM_WORKTREE" "$UPSTREAM_BRANCH"
  else
    git worktree add -q --detach "$UPSTREAM_WORKTREE"
    (wt_git checkout --orphan "$UPSTREAM_BRANCH" -q && wt_git rm -rf -q . >/dev/null 2>&1 || true)
  fi

  find "$UPSTREAM_WORKTREE" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
  mkdir "$UPSTREAM_WORKTREE/make"
  cp -R "$SRC_MAKE"/. "$UPSTREAM_WORKTREE/make/"

  rc=0
  (
    wt_git add -A
    if wt_git diff --cached --quiet; then
      exit 1
    fi
    wt_git commit -q -m "Sync upstream make as of $(date +%Y-%m-%d)" \
      -m "openbsd/src $(git -C "$SRC_ROOT" log -1 --format='%H (%cs)')"
  ) || rc=$?

  git worktree remove --force "$UPSTREAM_WORKTREE" >/dev/null 2>&1 || true
  return $rc
}
