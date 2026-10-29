#!/bin/bash
# tools/land.sh: land hsed work where a wapex re-sync cannot discard it.
# docs/spec-as-source.md section 10.4 explains why: the everyday re-sync runs
# `git submodule update --init --force`, which leaves hsed detached and throws
# away its index and working tree. Commits survive only once they are on
# GitHub AND the home repo's pointer to hsed has been bumped and pushed to wapex.
#
#   tools/land.sh [-n] [-F] [-m MESSAGE] [--] [PATH ...]
#
#   1. hsed must be on feature/types (detached HEAD is checked out as that branch)
#   2. PATHs, if any, are staged (explicit paths only, never -A); staged changes
#      are committed with -m MESSAGE
#   3. git push origin HEAD:feature/types            (GitHub)
#   4. refuse to go on unless origin/feature/types contains HEAD
#   5. home repo, on branch work: git add hsed; commit "hsed: <subject>"; push origin work (wapex)
#
#   -n, --dry-run   run every check; print the state-changing commands instead of running them
#   -F, --no-fetch  skip the fetch before step 4 (the remote-tracking ref may be stale)
#
# Commit messages are taken verbatim; nothing is appended to them.
set -euo pipefail

die()   { printf 'land.sh: %s\n' "$*" >&2; exit 1; }
note()  { printf 'land.sh: %s\n' "$*"; }
run()   { if $dry; then printf '[dry-run] %s\n' "$*"; else "$@"; fi; }

branch=feature/types
dry=false; fetch=true; msg=''
while [ $# -gt 0 ]; do
    case "$1" in
        -n|--dry-run)  dry=true ;;
        -F|--no-fetch) fetch=false ;;
        -m)            [ $# -ge 2 ] || die "-m needs a message"; msg=$2; shift ;;
        --)            shift; break ;;
        -*)            die "unknown option $1" ;;
        *)             break ;;
    esac
    shift
done
paths=("$@")

hsed=$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)
home=$(git -C "$hsed" rev-parse --show-superproject-working-tree)
[ -n "$home" ] || die "$hsed is not a submodule of anything; nothing to bump"
grep -q '^\[submodule "hsed"\]' "$home/.gitmodules" || die "$home/.gitmodules does not list hsed"

# 1. on the branch
if current=$(git -C "$hsed" symbolic-ref -q --short HEAD); then
    [ "$current" = "$branch" ] || die "hsed is on $current, not $branch; land from $branch"
else
    note "hsed is detached (a re-sync does that); restoring $branch at HEAD"
    run git -C "$hsed" checkout -B "$branch"
fi

# 2. stage and commit
[ ${#paths[@]} -eq 0 ] || run git -C "$hsed" add -- "${paths[@]}"
if ! git -C "$hsed" diff --cached --quiet || { $dry && [ ${#paths[@]} -gt 0 ]; }; then
    [ -n "$msg" ] || die "there are staged changes but no -m MESSAGE"
    run git -C "$hsed" commit -m "$msg"
fi
left=$(git -C "$hsed" status --short)
[ -z "$left" ] || note "not landed (unstaged or untracked):"$'\n'"$left"

# 3. push to GitHub
! $fetch || git -C "$hsed" fetch -q origin
run git -C "$hsed" push origin "HEAD:$branch"

# 4. the pointer may only name a commit GitHub has
head=$(git -C "$hsed" rev-parse HEAD)
git -C "$hsed" merge-base --is-ancestor "$head" "origin/$branch" \
    || die "refusing to bump the pointer: ${head:0:7} is not on origin/$branch (push first)"

# 5. bump the pointer on work and push it to wapex
[ "$(git -C "$home" rev-parse --abbrev-ref HEAD)" = work ] || die "home repo is not on work"
[ "$(git -C "$home" config --get submodule.hsed.update || true)" = checkout ] \
    || note "hint: git -C $home config submodule.hsed.update checkout (re-syncs leave hsed detached otherwise)"
recorded=$(git -C "$home" ls-tree HEAD hsed | awk '{print $3}')
if [ "$recorded" = "$head" ]; then
    note "pointer already at ${head:0:7}"
else
    subject=$(git -C "$hsed" log -1 --format=%s)
    run git -C "$home" add hsed
    run git -C "$home" commit -m "hsed: $subject" -- hsed
fi
run git -C "$home" push origin work
note "landed ${head:0:7} on $branch"
