#!/usr/bin/env bash
# Tests for modules/agentic/hooks/vcs-guard
# See .agents/issues/01-jj-or-bust-hook.md and .agents/PRD_DETERMINISTIC_VCS.md
#
# Usage: ./tests/vcs-guard.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../vcs-guard"
PASS=0
FAIL=0

# Fixture directories: one with .jj, one without
JJ_REPO=$(mktemp -d)
PLAIN_REPO=$(mktemp -d)
mkdir -p "$JJ_REPO/.jj"
trap 'rm -rf "$JJ_REPO" "$PLAIN_REPO"' EXIT

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run_hook() {
  local input="$1"
  printf '%s' "$input" | "$HOOK"
}

# Make a hook input JSON for the given command and cwd.
make_input() {
  local cmd="$1" cwd="$2"
  jq -n --arg cmd "$cmd" --arg cwd "$cwd" \
    '{"tool_name":"Bash","tool_input":{"command":$cmd},"cwd":$cwd}'
}

# Normalise JSON to compact form for comparison.
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

decision_of() {
  jq -r '.hookSpecificOutput.permissionDecision // empty'
}

reason_of() {
  jq -r '.hookSpecificOutput.permissionDecisionReason // empty'
}

# ---------------------------------------------------------------------------
# Tests: non-VCS commands always defer
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "ls" "$JJ_REPO")" | compact)
assert_eq "ls in jj repo: defer" "{}" "$result"

result=$(run_hook "$(make_input "ls -la" "$PLAIN_REPO")" | compact)
assert_eq "ls -la outside jj repo: defer" "{}" "$result"

result=$(run_hook "$(make_input "cat /etc/hosts" "$JJ_REPO")" | compact)
assert_eq "cat: defer" "{}" "$result"

# ---------------------------------------------------------------------------
# Tests: git in jj repo — deny write commands
# ---------------------------------------------------------------------------

# git push → deny; reason must name jj git push
result=$(run_hook "$(make_input "git push" "$JJ_REPO")")
assert_eq "git push in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "git push in jj repo: reason names jj git push" \
  "jj git push" "$(printf '%s' "$result" | reason_of)"

# git push origin main → deny
result=$(run_hook "$(make_input "git push origin main" "$JJ_REPO")")
assert_eq "git push origin main in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# git commit → deny; reason must name jj commit
result=$(run_hook "$(make_input 'git commit -m "initial"' "$JJ_REPO")")
assert_eq "git commit in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "git commit in jj repo: reason names jj commit" \
  "jj commit" "$(printf '%s' "$result" | reason_of)"

# git stash → deny; reason must be specific (not generic), mentioning jj new
result=$(run_hook "$(make_input "git stash" "$JJ_REPO")")
assert_eq "git stash in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "git stash in jj repo: reason mentions jj new" \
  "jj new" "$(printf '%s' "$result" | reason_of)"

# git checkout → deny; reason must name jj edit or jj new
result=$(run_hook "$(make_input "git checkout main" "$JJ_REPO")")
assert_eq "git checkout in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# git rebase → deny; reason names jj rebase
result=$(run_hook "$(make_input "git rebase origin/main" "$JJ_REPO")")
assert_eq "git rebase in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "git rebase in jj repo: reason names jj rebase" \
  "jj rebase" "$(printf '%s' "$result" | reason_of)"

# git config without read flag → deny
result=$(run_hook "$(make_input "git config user.email foo@example.com" "$JJ_REPO")")
assert_eq "git config write in jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Tests: git in jj repo — allow read-only commands
# ---------------------------------------------------------------------------

# git log → allow
result=$(run_hook "$(make_input "git log --oneline -5" "$JJ_REPO")")
assert_eq "git log in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git diff → allow
result=$(run_hook "$(make_input "git diff HEAD" "$JJ_REPO")")
assert_eq "git diff in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git show → allow
result=$(run_hook "$(make_input "git show HEAD" "$JJ_REPO")")
assert_eq "git show in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git status → allow
result=$(run_hook "$(make_input "git status" "$JJ_REPO")")
assert_eq "git status in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git blame → allow
result=$(run_hook "$(make_input "git blame README.md" "$JJ_REPO")")
assert_eq "git blame in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git rev-parse → allow
result=$(run_hook "$(make_input "git rev-parse HEAD" "$JJ_REPO")")
assert_eq "git rev-parse in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git ls-files → allow
result=$(run_hook "$(make_input "git ls-files" "$JJ_REPO")")
assert_eq "git ls-files in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git config --get → allow
result=$(run_hook "$(make_input "git config --get user.email" "$JJ_REPO")")
assert_eq "git config --get in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git config --list → allow
result=$(run_hook "$(make_input "git config --list" "$JJ_REPO")")
assert_eq "git config --list in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# git config -l → allow
result=$(run_hook "$(make_input "git config -l" "$JJ_REPO")")
assert_eq "git config -l in jj repo: allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Tests: git outside jj repo — defer (except reset --hard)
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "git push" "$PLAIN_REPO")" | compact)
assert_eq "git push outside jj repo: defer" "{}" "$result"

result=$(run_hook "$(make_input "git commit -m x" "$PLAIN_REPO")" | compact)
assert_eq "git commit outside jj repo: defer" "{}" "$result"

result=$(run_hook "$(make_input "git log --oneline" "$PLAIN_REPO")" | compact)
assert_eq "git log outside jj repo: defer" "{}" "$result"

# ---------------------------------------------------------------------------
# Tests: git reset --hard — always ask, regardless of jj context
# See .agents/issues/03-git-reset-hard-ask.md and .agents/PRD_DETERMINISTIC_VCS.md
# The ask check runs before .jj detection so it fires in both pure-git and
# jj-colocated repos.
# ---------------------------------------------------------------------------

# Outside jj repo: ask
result=$(run_hook "$(make_input "git reset --hard origin/main" "$PLAIN_REPO")")
assert_eq "git reset --hard outside jj repo: ask" \
  "ask" "$(printf '%s' "$result" | decision_of)"

result=$(run_hook "$(make_input "git reset --hard HEAD~1" "$PLAIN_REPO")")
assert_eq "git reset --hard HEAD~1 outside jj repo: ask" \
  "ask" "$(printf '%s' "$result" | decision_of)"

# Inside jj repo: ask (ask check precedes the jj-or-bust deny)
result=$(run_hook "$(make_input "git reset --hard HEAD~1" "$JJ_REPO")")
assert_eq "git reset --hard HEAD~1 inside jj repo: ask" \
  "ask" "$(printf '%s' "$result" | decision_of)"

result=$(run_hook "$(make_input "git reset --hard origin/main" "$JJ_REPO")")
assert_eq "git reset --hard origin/main inside jj repo: ask" \
  "ask" "$(printf '%s' "$result" | decision_of)"

# git reset without --hard: outside jj repo defer, inside jj repo deny
result=$(run_hook "$(make_input "git reset HEAD" "$PLAIN_REPO")" | compact)
assert_eq "git reset (no --hard) outside jj repo: defer" "{}" "$result"

result=$(run_hook "$(make_input "git reset --soft HEAD~1" "$JJ_REPO")")
assert_eq "git reset --soft inside jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

result=$(run_hook "$(make_input "git reset HEAD~1" "$JJ_REPO")")
assert_eq "git reset (no --hard) inside jj repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Tests: .jj detection walks up from child directories
# See .agents/issues/01-jj-or-bust-hook.md: walk up from cwd to filesystem root
# ---------------------------------------------------------------------------

CHILD_DIR="$JJ_REPO/subdir/deep"
mkdir -p "$CHILD_DIR"

result=$(run_hook "$(make_input "git push" "$CHILD_DIR")")
assert_eq ".jj detected walking up: git push from subdir deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

result=$(run_hook "$(make_input "git log" "$CHILD_DIR")")
assert_eq ".jj detected walking up: git log from subdir allow" \
  "allow" "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Tests: exit-0 invariant
# The hook contract requires exit 0 on every invocation.
# See .agents/PRD_DETERMINISTIC_VCS.md: "exit 0 on every invocation"
# ---------------------------------------------------------------------------

# Closing stdout forces jq inside emit_decision and printf inside emit_defer
# to fail. The hook must still exit 0 in both cases.

input=$(make_input "git push" "$JJ_REPO")
printf '%s' "$input" | "$HOOK" >&- 2>/dev/null
assert_eq "exit-0 invariant: emit_decision path (stdout closed)" "0" "$?"

printf '{"tool_input":{"command":"ls"},"cwd":"/"}' | "$HOOK" >&- 2>/dev/null
assert_eq "exit-0 invariant: emit_defer path (stdout closed)" "0" "$?"

# ---------------------------------------------------------------------------
# Tests: bare 'git' with no subcommand
# See review: bare git produced `git ` (trailing-space backtick span)
# ---------------------------------------------------------------------------

result=$(run_hook "$(make_input "git" "$JJ_REPO")")
decision=$(printf '%s' "$result" | decision_of)
reason=$(printf '%s' "$result" | reason_of)
assert_eq "bare git in jj repo: deny" "deny" "$decision"
# Reason must not contain a backtick span with a trailing space like `git `
if [[ "$reason" == *'`git `'* ]]; then
  echo "FAIL: bare git in jj repo: reason contains malformed \`git \` span"
  ((FAIL++))
else
  echo "PASS: bare git in jj repo: reason does not contain \`git \` span"
  ((PASS++))
fi

# ---------------------------------------------------------------------------
# Tests: jj interactive-command guard
# See .agents/issues/02-interactive-jj-block.md and .agents/PRD_DETERMINISTIC_VCS.md
# ---------------------------------------------------------------------------

# jj squash without flags → deny; reason must mention -m or -u
result=$(run_hook "$(make_input "jj squash" "$JJ_REPO")")
assert_eq "jj squash without flags: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
reason=$(printf '%s' "$result" | reason_of)
if [[ "$reason" == *"needs -m/-u"* ]]; then
  echo "PASS: jj squash without flags: reason mentions needs -m/-u"
  ((PASS++))
else
  echo "FAIL: jj squash without flags: reason does not mention needs -m/-u"
  echo "  actual reason: $reason"
  ((FAIL++))
fi

# jj squash -r SRC --into DEST -u → deny; reason names --from and incompatible
result=$(run_hook "$(make_input "jj squash -r X --into Y -u" "$JJ_REPO")")
assert_eq "jj squash -r X --into Y -u: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
reason=$(printf '%s' "$result" | reason_of)
assert_contains "jj squash -r + --into: reason names --from" \
  "--from" "$reason"
assert_contains "jj squash -r + --into: reason says incompatible" \
  "incompatible" "$reason"

# jj squash --revision X --into Y -m foo → deny (long form + --into)
result=$(run_hook "$(make_input "jj squash --revision X --into Y -m foo" "$JJ_REPO")")
assert_eq "jj squash --revision X --into Y -m foo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "jj squash --revision + --into: reason names --from" \
  "--from" "$(printf '%s' "$result" | reason_of)"

# jj squash -r X -t Y -u → deny (-t is short alias for --into)
result=$(run_hook "$(make_input "jj squash -r X -t Y -u" "$JJ_REPO")")
assert_eq "jj squash -r X -t Y -u: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
assert_contains "jj squash -r + -t: reason names --from" \
  "--from" "$(printf '%s' "$result" | reason_of)"

# jj squash --from X --into Y -u → defer (correct multi-target form)
result=$(run_hook "$(make_input "jj squash --from X --into Y -u" "$JJ_REPO")" | compact)
assert_eq "jj squash --from X --into Y -u: defer" "{}" "$result"

# jj squash valid chain: -r from a later chained command must not bleed into
# the squash check and trigger the -r/--into incompatibility denial.
result=$(run_hook "$(make_input "jj squash --from @ --into pvoosxnx --use-destination-message && jj status && jj log -r '::@ & ~root()' --limit 5" "$JJ_REPO")" | compact)
assert_eq "jj squash valid chain with downstream -r: defer" "{}" "$result"

# jj squash --from X --into Y -m foo → defer
result=$(run_hook "$(make_input "jj squash --from X --into Y -m foo" "$JJ_REPO")" | compact)
assert_eq "jj squash --from X --into Y -m foo: defer" "{}" "$result"

# jj squash -m foo → defer
result=$(run_hook "$(make_input "jj squash -m foo" "$JJ_REPO")" | compact)
assert_eq "jj squash -m foo: defer" "{}" "$result"

# jj squash -u → defer
result=$(run_hook "$(make_input "jj squash -u" "$JJ_REPO")" | compact)
assert_eq "jj squash -u: defer" "{}" "$result"

# jj squash --message foo → defer
result=$(run_hook "$(make_input "jj squash --message foo" "$JJ_REPO")" | compact)
assert_eq "jj squash --message foo: defer" "{}" "$result"

# jj squash --use-destination-message → defer
result=$(run_hook "$(make_input "jj squash --use-destination-message" "$JJ_REPO")" | compact)
assert_eq "jj squash --use-destination-message: defer" "{}" "$result"

# jj describe without flags → deny
result=$(run_hook "$(make_input "jj describe" "$JJ_REPO")")
assert_eq "jj describe without flags: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj describe -m 'msg' → defer
result=$(run_hook "$(make_input "jj describe -m 'my commit message'" "$JJ_REPO")" | compact)
assert_eq "jj describe -m msg: defer" "{}" "$result"

# jj describe --message msg → defer
result=$(run_hook "$(make_input "jj describe --message msg" "$JJ_REPO")" | compact)
assert_eq "jj describe --message msg: defer" "{}" "$result"

# jj commit without flags → deny
result=$(run_hook "$(make_input "jj commit" "$JJ_REPO")")
assert_eq "jj commit without flags: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj commit -m 'msg' → defer
result=$(run_hook "$(make_input "jj commit -m 'my commit'" "$JJ_REPO")" | compact)
assert_eq "jj commit -m msg: defer" "{}" "$result"

# jj split without flags → deny
result=$(run_hook "$(make_input "jj split" "$JJ_REPO")")
assert_eq "jj split without flags: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split -m foo has no fileset — still opens interactive split-point editor
result=$(run_hook "$(make_input "jj split -m foo" "$JJ_REPO")")
assert_eq "jj split -m foo (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split path/foo → defer (fileset provided)
result=$(run_hook "$(make_input "jj split path/foo" "$JJ_REPO")" | compact)
assert_eq "jj split path/foo (fileset provided): defer" "{}" "$result"

# jj split -r @- → deny (flag-only; -r value does not count as fileset positional)
result=$(run_hook "$(make_input "jj split -r @-" "$JJ_REPO")")
assert_eq "jj split -r @-: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
reason=$(printf '%s' "$result" | reason_of)
assert_contains "jj split -r @-: reason mentions FILESETS" \
  "FILESETS" "$reason"

# jj split -r @- path/foo → defer (fileset provided alongside revision flag)
result=$(run_hook "$(make_input "jj split -r @- path/foo" "$JJ_REPO")" | compact)
assert_eq "jj split -r @- path/foo: defer" "{}" "$result"

# jj split --tool vimdiff → deny (tool name is a value token, not a fileset)
result=$(run_hook "$(make_input "jj split --tool vimdiff" "$JJ_REPO")")
assert_eq "jj split --tool vimdiff (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split --tool=vimdiff (attached form) → deny
result=$(run_hook "$(make_input "jj split --tool=vimdiff" "$JJ_REPO")")
assert_eq "jj split --tool=vimdiff (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split -o abc123 → deny (-o/--onto value does not count as fileset)
result=$(run_hook "$(make_input "jj split -o abc123" "$JJ_REPO")")
assert_eq "jj split -o abc123 (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split --onto abc123 → deny (space form)
result=$(run_hook "$(make_input "jj split --onto abc123" "$JJ_REPO")")
assert_eq "jj split --onto abc123 (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj split --revision abc123 → deny (long form of -r)
result=$(run_hook "$(make_input "jj split --revision abc123" "$JJ_REPO")")
assert_eq "jj split --revision abc123 (no fileset): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj metaedit without flags → deny
result=$(run_hook "$(make_input "jj metaedit" "$JJ_REPO")")
assert_eq "jj metaedit without flags: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj metaedit -m msg → defer
result=$(run_hook "$(make_input "jj metaedit -m msg" "$JJ_REPO")" | compact)
assert_eq "jj metaedit -m msg: defer" "{}" "$result"

# jj diffedit → deny (unconditional)
result=$(run_hook "$(make_input "jj diffedit" "$JJ_REPO")")
assert_eq "jj diffedit: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj arrange → deny (unconditional; always interactive)
result=$(run_hook "$(make_input "jj arrange" "$JJ_REPO")")
assert_eq "jj arrange: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj resolve without --tool → deny
result=$(run_hook "$(make_input "jj resolve" "$JJ_REPO")")
assert_eq "jj resolve without --tool: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj resolve --tool kdiff3 → defer
result=$(run_hook "$(make_input "jj resolve --tool kdiff3" "$JJ_REPO")" | compact)
assert_eq "jj resolve --tool kdiff3: defer" "{}" "$result"

# jj resolve --tool=kdiff3 (--flag=value form) → defer
result=$(run_hook "$(make_input "jj resolve --tool=kdiff3" "$JJ_REPO")" | compact)
assert_eq "jj resolve --tool=kdiff3: defer" "{}" "$result"

# ---------------------------------------------------------------------------
# Tests: jj new bare-command guard
# Bare jj new (no positional revision, no -m, no -A, no -B) creates an empty
# commit on @ and is the giveaway of the describe-then-new anti-pattern.
# Any deliberate-intent indicator defers to the normal permission system.
# ---------------------------------------------------------------------------

# jj new with no arguments → deny; reason must name both jj commit -m and jj new <rev>
result=$(run_hook "$(make_input "jj new" "$JJ_REPO")")
assert_eq "jj new bare: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"
reason=$(printf '%s' "$result" | reason_of)
assert_contains "jj new bare: reason names jj commit -m" \
  "jj commit -m" "$reason"
assert_contains "jj new bare: reason names jj new <rev>" \
  "jj new <rev>" "$reason"

# jj new abc123 → defer (positional revision provided)
result=$(run_hook "$(make_input "jj new abc123" "$JJ_REPO")" | compact)
assert_eq "jj new abc123: defer" "{}" "$result"

# jj new -m "msg" → defer (-m flag present)
result=$(run_hook "$(make_input 'jj new -m "msg"' "$JJ_REPO")" | compact)
assert_eq "jj new -m msg: defer" "{}" "$result"

# jj new --message "msg" → defer (--message long form)
result=$(run_hook "$(make_input 'jj new --message "msg"' "$JJ_REPO")" | compact)
assert_eq "jj new --message msg: defer" "{}" "$result"

# jj new -A @- → defer (-A/--insert-after flag present; value token @- is skipped)
result=$(run_hook "$(make_input "jj new -A @-" "$JJ_REPO")" | compact)
assert_eq "jj new -A @-: defer" "{}" "$result"

# jj new -B @- → defer (-B/--insert-before flag present; value token @- is skipped)
result=$(run_hook "$(make_input "jj new -B @-" "$JJ_REPO")" | compact)
assert_eq "jj new -B @-: defer" "{}" "$result"

# jj new --insert-after @- → defer (long form of -A)
result=$(run_hook "$(make_input "jj new --insert-after @-" "$JJ_REPO")" | compact)
assert_eq "jj new --insert-after @-: defer" "{}" "$result"

# jj new --insert-before @- → defer (long form of -B)
result=$(run_hook "$(make_input "jj new --insert-before @-" "$JJ_REPO")" | compact)
assert_eq "jj new --insert-before @-: defer" "{}" "$result"

# jj new -R /path/to/repo → deny (global -R flag value must not be counted as positional)
result=$(run_hook "$(make_input "jj new -R /path/to/repo" "$JJ_REPO")")
assert_eq "jj new -R /repo (global flag, no revision): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj new --no-edit → deny (--no-edit is a boolean flag, not a revision)
result=$(run_hook "$(make_input "jj new --no-edit" "$JJ_REPO")")
assert_eq "jj new --no-edit (no revision): deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# jj log → defer (non-interactive, not in deny list)
result=$(run_hook "$(make_input "jj log" "$JJ_REPO")" | compact)
assert_eq "jj log: defer" "{}" "$result"

# jj interactive-command denials apply in plain repos too (not jj-specific)
result=$(run_hook "$(make_input "jj squash" "$PLAIN_REPO")")
assert_eq "jj squash in plain repo: deny" \
  "deny" "$(printf '%s' "$result" | decision_of)"

# ---------------------------------------------------------------------------
# Tests: jj --help / -h always defers regardless of subcommand
# --help is informational and never opens $EDITOR; blocking it prevents
# legitimate introspection.
# See .agents/PRD_DETERMINISTIC_VCS.md
# ---------------------------------------------------------------------------

# squash --help → defer (even though squash without flags would deny)
result=$(run_hook "$(make_input "jj squash --help" "$JJ_REPO")" | compact)
assert_eq "jj squash --help: defer" "{}" "$result"

# describe --help → defer
result=$(run_hook "$(make_input "jj describe --help" "$JJ_REPO")" | compact)
assert_eq "jj describe --help: defer" "{}" "$result"

# diffedit --help → defer (even though diffedit is unconditionally denied)
result=$(run_hook "$(make_input "jj diffedit --help" "$JJ_REPO")" | compact)
assert_eq "jj diffedit --help: defer" "{}" "$result"

# commit --help → defer
result=$(run_hook "$(make_input "jj commit --help" "$JJ_REPO")" | compact)
assert_eq "jj commit --help: defer" "{}" "$result"

# -h short form → defer (squash: flag-checked command)
result=$(run_hook "$(make_input "jj squash -h" "$JJ_REPO")" | compact)
assert_eq "jj squash -h: defer" "{}" "$result"

# -h short form → defer (describe: another flag-checked command; verifies -h is subcommand-agnostic)
result=$(run_hook "$(make_input "jj describe -h" "$JJ_REPO")" | compact)
assert_eq "jj describe -h: defer" "{}" "$result"

# -h short form → defer (diffedit: unconditionally-denied command; verifies -h overrides even hard denials)
result=$(run_hook "$(make_input "jj diffedit -h" "$JJ_REPO")" | compact)
assert_eq "jj diffedit -h: defer" "{}" "$result"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]]
