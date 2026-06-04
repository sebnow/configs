#!/usr/bin/env bash
# Tests for modules/agentic/hooks/obsidian-cli-guard
#
# Usage: ./tests/obsidian-cli-guard.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../obsidian-cli-guard"
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
# Deny: obsidian-cli invoked with dash-prefixed tokens
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create --help" \
  "obsidian-cli create --vault X --path Y" \
  "obsidian-cli search -h" \
  "obsidian-cli create path=Notes/Foo.md --extra-flag" \
  "obsidian-cli vaults --verbose"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny: $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# Deny after segment boundaries: the segment-start anchor prevents false
# positives where obsidian-cli appears as a quoted string argument to echo.
for cmd in \
  "cd /tmp && obsidian-cli create --help" \
  "true ; obsidian-cli create --help" \
  "(obsidian-cli create --help)"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (segment boundary): $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# ---------------------------------------------------------------------------
# Deny-reason: message must name the failure mode and corrective action
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "obsidian-cli create --help")")
reason=$(printf '%s' "$result" | reason_of)
assert_contains "deny-reason: contains key=value" "key=value" "$reason"
assert_contains "deny-reason: names Untitled.md failure mode" "Untitled.md" "$reason"
assert_contains "deny-reason: names corrective obsidian-cli help subcommand" "help" "$reason"

# ---------------------------------------------------------------------------
# Defer: valid obsidian-cli invocations with no dash-prefixed tokens
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create path=\"Notes/Foo.md\" content=\"...\"" \
  "obsidian-cli help create" \
  "obsidian-cli search query=\"foo\" vault=Knowledge" \
  "obsidian-cli vaults verbose" \
  "obsidian-cli read path=\"Notes/Foo.md\" vault=Knowledge"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer: $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Defer: dash flags on commands other than obsidian-cli must not be blocked
# ---------------------------------------------------------------------------

for cmd in \
  "echo --help | grep --color" \
  "grep --color=auto foo bar.txt" \
  "find . -name '*.md' -type f" \
  "git log --oneline"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (other cmd): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Defer: obsidian-cli as quoted string inside echo — segment-start anchoring
# prevents matching when the binary name follows another command's argument.
# We accept the documented limitation (same as block-test-output-filtering):
# if obsidian-cli is at a true segment start inside quotes we may not catch it.
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input 'echo "obsidian-cli create --help"')" | compact)
# The binary appears as an argument to echo, not at a segment start — should defer.
# NOTE: this is a best-effort case; the hook uses regex not a shell parser.
assert_eq "defer: echo with obsidian-cli string as argument" "{}" "$result"

# ---------------------------------------------------------------------------
# Word-boundary: obsidian-cli-guard itself and other prefixed names must not trip
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli-guard --help" \
  "obsidian-cli-backup --vault X"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (word boundary): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Exit-0 invariant: hook must exit 0 in every path, even on stdout failure
# ---------------------------------------------------------------------------

input=$(make_input "obsidian-cli create --help")
printf '%s' "$input" | "$HOOK" >&- 2>/dev/null
assert_eq "exit-0: emit_decision path (stdout closed)" "0" "$?"

printf '{"tool_input":{"command":"obsidian-cli vaults verbose"}}' | "$HOOK" >&- 2>/dev/null
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
