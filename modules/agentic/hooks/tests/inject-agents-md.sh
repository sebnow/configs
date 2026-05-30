#!/usr/bin/env bash
# Tests for modules/agentic/hooks/inject-agents-md
#
# Usage: ./tests/inject-agents-md.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../inject-agents-md"
PASS=0
FAIL=0

TMP_ROOT=$(mktemp -d)
trap 'rm -rf "$TMP_ROOT"' EXIT

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run_hook() {
  local input="$1"
  printf '%s' "$input" | "$HOOK"
}

make_input() {
  local cwd="$1"
  jq -n --arg cwd "$cwd" '{"cwd":$cwd}'
}

compact() {
  jq -c .
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

assert_not_contains() {
  local desc="$1" needle="$2" haystack="$3"
  if [[ "$haystack" != *"$needle"* ]]; then
    echo "PASS: $desc"
    ((PASS++))
  else
    echo "FAIL: $desc"
    echo "  expected NOT to contain: $needle"
    echo "  actual: $haystack"
    ((FAIL++))
  fi
}

context_of() {
  jq -r '.hookSpecificOutput.additionalContext // empty'
}

# Fresh directory for each test case.
new_dir() {
  mktemp -d -p "$TMP_ROOT"
}

# ---------------------------------------------------------------------------
# Case 1: No AGENTS.md anywhere → exit 0, no output (or {}).
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: no AGENTS.md: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "no AGENTS.md: empty or {}" "{}" "$compacted"
fi

# ---------------------------------------------------------------------------
# Case 2: AGENTS.md in cwd (cwd is project root) → emits framed content.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
printf 'hello from agents.md\n' > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "cwd is root: header names AGENTS.md path" \
  "$dir/AGENTS.md" "$ctx"
assert_contains "cwd is root: header has 'project AGENTS.md'" \
  "project AGENTS.md" "$ctx"
assert_contains "cwd is root: emits content body" \
  "hello from agents.md" "$ctx"

# ---------------------------------------------------------------------------
# Case 3: AGENTS.md in ancestor with .git marker, cwd is subdir.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git" "$dir/sub/deep"
printf 'top-level agents content\n' > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir/sub/deep")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "ancestor lookup: finds AGENTS.md via .git walk" \
  "$dir/AGENTS.md" "$ctx"
assert_contains "ancestor lookup: emits content" \
  "top-level agents content" "$ctx"

# ---------------------------------------------------------------------------
# Case 4: AGENTS.md symlinks to CLAUDE.md → skip.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
printf 'shared content\n' > "$dir/CLAUDE.md"
ln -s CLAUDE.md "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: AGENTS.md -> CLAUDE.md symlink: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "AGENTS.md -> CLAUDE.md symlink: empty or {}" "{}" "$compacted"
fi

# ---------------------------------------------------------------------------
# Case 5: CLAUDE.md symlinks to AGENTS.md → skip.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
printf 'shared content\n' > "$dir/AGENTS.md"
ln -s AGENTS.md "$dir/CLAUDE.md"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: CLAUDE.md -> AGENTS.md symlink: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "CLAUDE.md -> AGENTS.md symlink: empty or {}" "{}" "$compacted"
fi

# ---------------------------------------------------------------------------
# Case 6: Both symlink to a third file → skip.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
printf 'third-party content\n' > "$dir/SHARED.md"
ln -s SHARED.md "$dir/AGENTS.md"
ln -s SHARED.md "$dir/CLAUDE.md"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: both symlinks to third file: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "both symlinks to third file: empty or {}" "{}" "$compacted"
fi

# ---------------------------------------------------------------------------
# Case 7: Both AGENTS.md and CLAUDE.md present, distinct files → emit AGENTS.md.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
printf 'agents-only content\n' > "$dir/AGENTS.md"
printf 'claude-only content\n' > "$dir/CLAUDE.md"
result=$(run_hook "$(make_input "$dir")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "distinct files: emits AGENTS.md content" \
  "agents-only content" "$ctx"
assert_not_contains "distinct files: does not emit CLAUDE.md content" \
  "claude-only content" "$ctx"

# ---------------------------------------------------------------------------
# Case 8: AGENTS.md empty / whitespace-only → skip.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
: > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: empty AGENTS.md: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "empty AGENTS.md: empty or {}" "{}" "$compacted"
fi

dir=$(new_dir)
mkdir -p "$dir/.git"
printf '   \n\t\n  \n' > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
if [[ -z "$result" ]]; then
  echo "PASS: whitespace-only AGENTS.md: empty output"
  ((PASS++))
else
  compacted=$(printf '%s' "$result" | compact)
  assert_eq "whitespace-only AGENTS.md: empty or {}" "{}" "$compacted"
fi

# ---------------------------------------------------------------------------
# Case 9: AGENTS.md > 200 lines → emits first 200 with truncation marker.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.git"
for i in $(seq 1 250); do printf 'line %d\n' "$i"; done > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "long file: includes first line" "line 1" "$ctx"
assert_contains "long file: includes 200th line" "line 200" "$ctx"
assert_not_contains "long file: excludes 201st line" "line 201" "$ctx"
assert_contains "long file: has truncation marker" \
  "[truncated" "$ctx"

# Exactly 200 lines → no truncation marker.
dir=$(new_dir)
mkdir -p "$dir/.git"
for i in $(seq 1 200); do printf 'line %d\n' "$i"; done > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "exactly 200 lines: includes line 200" "line 200" "$ctx"
assert_not_contains "exactly 200 lines: no truncation marker" \
  "[truncated" "$ctx"

# ---------------------------------------------------------------------------
# Case 10: .cwd missing or directory doesn't exist → exit 0 cleanly.
# ---------------------------------------------------------------------------

result=$(printf '{}' | "$HOOK"; echo "exit=$?")
assert_contains "missing cwd: exit 0" "exit=0" "$result"

result=$(printf '{"cwd":"/nonexistent/path/that/does/not/exist"}' | "$HOOK"; echo "exit=$?")
assert_contains "nonexistent cwd dir: exit 0" "exit=0" "$result"

# ---------------------------------------------------------------------------
# Case 11: No VCS marker anywhere → falls back to cwd as root.
# ---------------------------------------------------------------------------

dir=$(new_dir)
printf 'no-vcs agents content\n' > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains "no VCS marker: cwd treated as root" \
  "$dir/AGENTS.md" "$ctx"
assert_contains "no VCS marker: emits content" \
  "no-vcs agents content" "$ctx"

# ---------------------------------------------------------------------------
# Bonus: .jj marker also triggers ancestor detection.
# ---------------------------------------------------------------------------

dir=$(new_dir)
mkdir -p "$dir/.jj" "$dir/sub"
printf 'jj-rooted content\n' > "$dir/AGENTS.md"
result=$(run_hook "$(make_input "$dir/sub")")
ctx=$(printf '%s' "$result" | context_of)
assert_contains ".jj ancestor: finds AGENTS.md" \
  "jj-rooted content" "$ctx"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]]
