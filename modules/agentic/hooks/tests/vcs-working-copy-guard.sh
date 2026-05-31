#!/usr/bin/env bash
# Tests for modules/agentic/hooks/vcs-working-copy-guard
#
# The hook runs at session end (Stop event) and warns when the current
# working directory has an unclean VCS state, so the user is not surprised
# by an auto-commit on their next file edit.
#
# Usage: ./tests/vcs-working-copy-guard.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../vcs-working-copy-guard"
PASS=0
FAIL=0

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run_hook() {
  local cwd="$1"
  # The hook reads and discards stdin; supply a minimal Stop event payload.
  printf '{"hook_event_name":"Stop","stop_hook_active":false}' \
    | (cd "$cwd" && "$HOOK")
}

assert_eq() {
  local desc="$1" expected="$2" actual="$3"
  if [[ "$actual" == "$expected" ]]; then
    echo "PASS: $desc"
    ((PASS++))
  else
    echo "FAIL: $desc"
    echo "  expected: $expected"
    echo "  actual:   $actual"
    ((FAIL++))
  fi
}

assert_contains() {
  local desc="$1" needle="$2" haystack="$3"
  if [[ "$haystack" == *"$needle"* ]]; then
    echo "PASS: $desc"
    ((PASS++))
  else
    echo "FAIL: $desc"
    echo "  expected to contain: $needle"
    echo "  actual: $haystack"
    ((FAIL++))
  fi
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Temp directory that is not a VCS repo at all
PLAIN_DIR=$(mktemp -d)

# Temp jj repo
JJ_REPO=$(mktemp -d)
(cd "$JJ_REPO" && jj git init 2>/dev/null)

# Temp git repo
GIT_REPO=$(mktemp -d)
(cd "$GIT_REPO" && git init 2>/dev/null && git commit --allow-empty -m "root" 2>/dev/null)

trap 'rm -rf "$PLAIN_DIR" "$JJ_REPO" "$GIT_REPO"' EXIT

# ---------------------------------------------------------------------------
# Tests: clean jj working copy emits nothing
# A session ending with an empty @ must not disturb the user.
# ---------------------------------------------------------------------------

result=$(run_hook "$JJ_REPO")
assert_eq "clean jj repo: no output" "" "$result"

# Exit code checked separately after collecting output
(cd "$JJ_REPO" && printf '{"hook_event_name":"Stop","stop_hook_active":false}' | "$HOOK") >/dev/null 2>&1
assert_eq "clean jj repo: exits 0" "0" "$?"

# ---------------------------------------------------------------------------
# Tests: dirty jj working copy emits a systemMessage mentioning jj
# A tracked change in @ means the agent left work uncommitted.
# ---------------------------------------------------------------------------

echo "hello" > "$JJ_REPO/dirty.txt"
(cd "$JJ_REPO" && jj file track dirty.txt 2>/dev/null)

result=$(run_hook "$JJ_REPO")
system_message=$(printf '%s' "$result" | jq -r '.systemMessage // empty' 2>/dev/null)
assert_contains "dirty jj repo: output contains systemMessage" \
  "systemMessage" "$result"
assert_contains "dirty jj repo: systemMessage mentions jj" \
  "jj" "$system_message"

(cd "$JJ_REPO" && printf '{"hook_event_name":"Stop","stop_hook_active":false}' | "$HOOK") >/dev/null 2>&1
assert_eq "dirty jj repo: exits 0" "0" "$?"

# ---------------------------------------------------------------------------
# Tests: clean git repo emits nothing
# ---------------------------------------------------------------------------

result=$(run_hook "$GIT_REPO")
assert_eq "clean git repo: no output" "" "$result"

(cd "$GIT_REPO" && printf '{"hook_event_name":"Stop","stop_hook_active":false}' | "$HOOK") >/dev/null 2>&1
assert_eq "clean git repo: exits 0" "0" "$?"

# ---------------------------------------------------------------------------
# Tests: dirty git repo emits a systemMessage mentioning git
# ---------------------------------------------------------------------------

echo "hello" > "$GIT_REPO/dirty.txt"
git -C "$GIT_REPO" add dirty.txt 2>/dev/null

result=$(run_hook "$GIT_REPO")
system_message=$(printf '%s' "$result" | jq -r '.systemMessage // empty' 2>/dev/null)
assert_contains "dirty git repo: output contains systemMessage" \
  "systemMessage" "$result"
assert_contains "dirty git repo: systemMessage mentions git" \
  "git" "$system_message"

(cd "$GIT_REPO" && printf '{"hook_event_name":"Stop","stop_hook_active":false}' | "$HOOK") >/dev/null 2>&1
assert_eq "dirty git repo: exits 0" "0" "$?"

# ---------------------------------------------------------------------------
# Tests: non-repo cwd emits nothing
# When neither jj nor git finds a repo, the hook is silent.
# ---------------------------------------------------------------------------

result=$(run_hook "$PLAIN_DIR")
assert_eq "non-repo cwd: no output" "" "$result"

(cd "$PLAIN_DIR" && printf '{"hook_event_name":"Stop","stop_hook_active":false}' | "$HOOK") >/dev/null 2>&1
assert_eq "non-repo cwd: exits 0" "0" "$?"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]]
