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
# Defer: valid obsidian-cli chained with another command that uses flags —
# the .* in the pattern must not cross shell operator boundaries, otherwise
# flags on the chained command trigger a false positive denial.
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create path=Notes/Foo.md && cat -n /tmp/output" \
  "obsidian-cli search query=foo && ls -la" \
  "obsidian-cli read path=Foo.md vault=K || echo -n failed" \
  "obsidian-cli create path=Notes/Foo.md ; ls -la"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (chained cmd with flags): $cmd" "{}" "$result"
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
# Deny: obsidian-cli create with no target argument (path=, name=, or file=)
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create" \
  "obsidian-cli create 2>&1 | head" \
  "cd /tmp && obsidian-cli create"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (no target): $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# ---------------------------------------------------------------------------
# Defer: obsidian-cli create with a recognised target argument
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create path=\"Notes/Foo.md\" content=\"x\"" \
  "obsidian-cli create file=Foo.md" \
  "obsidian-cli create name=Foo"
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (has target): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Deny-reason: no-target message must name the Untitled.md failure mode
# and supply the corrective path= form
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "obsidian-cli create")")
reason=$(printf '%s' "$result" | reason_of)
assert_contains "deny-reason (no target): mentions path=" "path=" "$reason"
assert_contains "deny-reason (no target): names Untitled.md failure mode" "Untitled.md" "$reason"

# ---------------------------------------------------------------------------
# Deny: obsidian-cli create with both name= and path= where a value has a dot
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create name=\"Foo.md\" path=\"Notes/\"" \
  "obsidian-cli create name=Foo path=Notes.dir/"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (name+path dot): $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# ---------------------------------------------------------------------------
# Defer: name= and path= together but no dot; or only one of the two present
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli create name=\"Foo\" path=\"Notes\"" \
  "obsidian-cli create path=\"Notes/Foo.md\"" \
  "obsidian-cli create name=\"Foo.md\""
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (name+path safe): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Defer: name= or path= appearing only inside a quoted value of another
# argument — should not count as a real name=/path= argument
# ---------------------------------------------------------------------------

for cmd in \
  'obsidian-cli create path=Notes/Foo.md content="name=bar"' \
  'obsidian-cli create name=Foo content="path=other"'
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (key=val inside quoted value): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Deny-reason: name+path dot message must explain directory nesting and
# direct the agent to use path= alone
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "obsidian-cli create name=\"Foo.md\" path=\"Notes/\"")")
reason=$(printf '%s' "$result" | reason_of)
assert_contains "deny-reason (name+path dot): mentions nesting or directory" "nest" "$reason"
assert_contains "deny-reason (name+path dot): corrective guidance uses path=" "path=" "$reason"

# ---------------------------------------------------------------------------
# Deny: obsidian-cli delete with no target argument (path= or file=)
# ---------------------------------------------------------------------------

for cmd in \
  "obsidian-cli delete" \
  "obsidian-cli delete 2>&1 | head" \
  "cd /tmp && obsidian-cli delete"
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (delete no target): $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# ---------------------------------------------------------------------------
# Defer: obsidian-cli delete with a recognised target argument
# ---------------------------------------------------------------------------

for cmd in \
  'obsidian-cli delete path="Notes/Foo.md"' \
  'obsidian-cli delete file="Foo"' \
  'obsidian-cli delete file=Foo'
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (delete has target): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Deny-reason: delete no-target message must name the active-note failure mode
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "obsidian-cli delete")")
reason=$(printf '%s' "$result" | reason_of)
assert_contains "deny-reason (delete no target): mentions active note" "active note" "$reason"
assert_contains "deny-reason (delete no target): mentions path=" "path=" "$reason"

# ---------------------------------------------------------------------------
# Argument-position discipline: path= or file= inside a quoted value of
# another argument must NOT count as a target.  Without the (?:^|\s) anchor
# before the key names, the \b word-boundary inside a negative lookahead
# would find "path=" inside content="path=fake" and incorrectly defer —
# leaving the agent free to run a targetless delete that trashes the active note.
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input 'obsidian-cli delete content="path=fake"')" )
assert_eq "deny (delete arg-position: path= inside quoted value is not a target)" "deny" \
  "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Argument-position discipline: path=, name=, or file= inside a quoted value
# of another argument must NOT count as a create target.  The same \b bypass
# that was fixed in the delete pattern applies here: content="path=fake" must
# not satisfy the no-target negative lookahead and cause the hook to defer.
# ---------------------------------------------------------------------------

for cmd in \
  'obsidian-cli create content="path=fake"' \
  'obsidian-cli create content="name=fake"' \
  'obsidian-cli create content="file=fake"'
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (create arg-position: key= inside quoted value is not a target): $cmd" "deny" \
    "$(printf '%s' "$result" | decision_of)"
done

# These remain defers: the target keys appear at argument position, not inside
# a quoted value of a different argument.
for cmd in \
  'obsidian-cli create path="Notes/Foo.md" content="..."' \
  'obsidian-cli create path=Notes/Foo.md content="name=bar"' \
  'obsidian-cli create file=Foo.md' \
  'obsidian-cli create name=Foo'
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (create arg-position: real target key): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Deny: obsidian-cli rename with name= value that does not end in .md
# ---------------------------------------------------------------------------

for cmd in \
  'obsidian-cli rename file="Foo" name="Bar"' \
  'obsidian-cli rename file="Foo" name="Ficus carica var. Brown Turkey"' \
  'obsidian-cli rename file=Foo name=Bar' \
  'cd /tmp && obsidian-cli rename file="Foo" name="Bar"' \
  'obsidian-cli rename file="Foo" name="Bar.md.tmp"'
do
  result=$(run_hook "$(make_input "$cmd")")
  assert_eq "deny (rename name not .md): $cmd" "deny" "$(printf '%s' "$result" | decision_of)"
done

# ---------------------------------------------------------------------------
# Defer: obsidian-cli rename with name= value that ends in .md
# ---------------------------------------------------------------------------

for cmd in \
  'obsidian-cli rename file="Foo" name="Bar.md"' \
  'obsidian-cli rename file="Foo" name="Ficus carica var. Brown Turkey.md"' \
  'obsidian-cli rename file="Foo" name=Bar.md' \
  'obsidian-cli tag name="#project"' \
  'obsidian-cli rename file="Foo" name="Bar.md" content="name=ignored"'
do
  result=$(run_hook "$(make_input "$cmd")" | compact)
  assert_eq "defer (rename name .md or other cmd): $cmd" "{}" "$result"
done

# ---------------------------------------------------------------------------
# Deny-reason: rename non-.md message must mention .md and file extension
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input 'obsidian-cli rename file="Foo" name="Bar"')")
reason=$(printf '%s' "$result" | reason_of)
assert_contains "deny-reason (rename name not .md): contains .md" ".md" "$reason"
assert_contains "deny-reason (rename name not .md): mentions extension" "extension" "$reason"

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "Passed: $PASS"
echo "Failed: $FAIL"
if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
