#!/usr/bin/env bash
# Tests for modules/agentic/hooks/block-test-output-filtering
#
# Usage: ./tests/block-test-output-filtering.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../block-test-output-filtering"
PASS=0
FAIL=0

run_hook() {
  local input="$1"
  printf '%s' "$input" | "$HOOK"
}

make_input() {
  local cmd="$1"
  jq -n --arg cmd "$cmd" \
    '{"tool_name":"Bash","tool_input":{"command":$cmd},"cwd":"/tmp"}'
}

compact() {
  jq -c .
}

decision_of() {
  jq -r '.hookSpecificOutput.permissionDecision // empty'
}

reason_of() {
  jq -r '.hookSpecificOutput.permissionDecisionReason // empty'
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
# Deny: source command piped to filter
# ---------------------------------------------------------------------------

for cmd in \
  "pytest | head" \
  "pytest -v | head -n 20" \
  "pytest 2>&1 | grep FAIL" \
  "go test ./... | head" \
  "go test -run TestFoo | tail -n 5" \
  "cargo test | grep panic" \
  "cargo check | head" \
  "cargo clippy | grep warning" \
  "npm test | head" \
  "npm run test | grep fail" \
  "npm run lint | head" \
  "npm run typecheck | head" \
  "yarn test | tail" \
  "pnpm test | grep FAIL" \
  "bun test | head" \
  "deno test | head" \
  "jest | head" \
  "vitest | head" \
  "mocha | head" \
  "tsc | head" \
  "tsc --noEmit | grep error" \
  "mypy . | head" \
  "pyright | grep error" \
  "biome check . | head" \
  "biome ci . | head" \
  "eslint src/ | head" \
  "ruff check . | head" \
  "pytest | rg fail" \
  "pytest | awk '/FAIL/'" \
  "pytest | sed -n 1,20p" \
  "pytest | cut -d: -f1" \
  "pytest | wc -l" \
  "pytest | tr -d '\\r'" \
  "pytest |& head" \
  "pytest 2>&1 |& head" \
  "cd src && pytest | head" \
  "(pytest | head)" \
  "true ; pytest | head" \
  "false || pytest | head"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny: $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# Reason must name the persisted-output mechanism.
result=$(run_hook "$(make_input "pytest | head")")
assert_contains "reason mentions persisted-output" \
  "persisted-output" "$(printf '%s' "$result" | reason_of)"
assert_contains "reason names tee as escape hatch" \
  "tee" "$(printf '%s' "$result" | reason_of)"

# ---------------------------------------------------------------------------
# Defer: no pipe, or pipe to non-filter
# ---------------------------------------------------------------------------

for cmd in \
  "pytest" \
  "pytest -v" \
  "pytest > /tmp/out.log" \
  "pytest 2> /tmp/err.log" \
  "pytest | tee /tmp/log" \
  "pytest | cat" \
  "go test ./..." \
  "cargo test --release" \
  "npm test" \
  "npm run test" \
  "tsc --noEmit" \
  "biome check ." \
  "ls -la" \
  "echo hello" \
  "cat file.txt | head" \
  "git log | head" \
  "ls | grep foo" \
  "find . -name '*.go' | head" \
  "ps aux | grep ssh" \
  "rg --files | head"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer: $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Defer: source name appears inside a quoted echo (false-positive guard)
# ---------------------------------------------------------------------------

# These look like the source command but are arguments to echo/printf. We
# accept that some of these (where `pytest` is the first token of a segment
# inside the quotes) WILL be flagged — true zero-false-positive needs a
# shell-grammar parser. We document the limitation rather than chase it.

result=$(run_hook "$(make_input 'echo "running tests" | head')" | compact)
assert_eq "defer: echo plain string piped to head" "{}" "$result"

# ---------------------------------------------------------------------------
# Word-boundary: longer names that contain a source name should NOT match
# ---------------------------------------------------------------------------

for cmd in \
  "pytestify | head" \
  "biomechanics | head" \
  "jester | head" \
  "mypyc | head" \
  "tscompile | head" \
  "ruffian | head"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (word boundary): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Defer: tee escape hatch — `source | tee FILE` is the documented allow path
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input 'pytest | tee /tmp/out.log')" | compact)
assert_eq "defer: pytest | tee" "{}" "$result"

# Note: `pytest | tee /tmp/log | head` is NOT caught by design — tee captures
# full output, so downstream filtering is harmless. Documented in the hook.
result=$(run_hook "$(make_input 'pytest | tee /tmp/log | head')" | compact)
assert_eq "defer: pytest | tee | head (tee captures full output)" "{}" "$result"

# ---------------------------------------------------------------------------
# Exit-0 invariant: hook must exit 0 in every path, even on stdout failure.
# ---------------------------------------------------------------------------

input=$(make_input "pytest | head")
printf '%s' "$input" | "$HOOK" >&- 2>/dev/null
assert_eq "exit-0: emit_decision path (stdout closed)" "0" "$?"

printf '{"tool_input":{"command":"ls"}}' | "$HOOK" >&- 2>/dev/null
assert_eq "exit-0: emit_defer path (stdout closed)" "0" "$?"

# Empty stdin
printf '' | "$HOOK" >/dev/null 2>&1
assert_eq "exit-0: empty stdin" "0" "$?"

# Malformed JSON
printf 'not json' | "$HOOK" >/dev/null 2>&1
assert_eq "exit-0: malformed JSON" "0" "$?"

# Missing tool_input.command field
printf '{}' | "$HOOK" >/dev/null 2>&1
assert_eq "exit-0: missing command field" "0" "$?"

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "Passed: $PASS"
echo "Failed: $FAIL"
if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
