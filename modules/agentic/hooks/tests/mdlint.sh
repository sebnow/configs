#!/usr/bin/env bash
# Tests for modules/agentic/hooks/mdlint
# See .agents/prd-markdown.md
#
# Usage: ./tests/mdlint.sh
# Exit 0 if all tests pass, 1 if any fail.

HOOK="$(cd "$(dirname "$0")" && pwd)/../mdlint"
PASS=0
FAIL=0

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run_hook() {
  local input="$1"
  printf '%s' "$input" | "$HOOK"
}

make_edit_input() {
  local file_path="$1" new_string="$2"
  jq -n --arg path "$file_path" --arg content "$new_string" \
    '{"tool_name":"Edit","tool_input":{"file_path":$path,"new_string":$content}}'
}

make_write_input() {
  local file_path="$1" content="$2"
  jq -n --arg path "$file_path" --arg content "$content" \
    '{"tool_name":"Write","tool_input":{"file_path":$path,"content":$content}}'
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
# Tests: Edit on .md with emoji → block (exit 2)
# See .agents/prd-markdown.md acceptance criteria
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "Hello ❤ world")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with heart emoji: exits 2" "2" "$status"
assert_eq "Edit .md with heart emoji: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md with heart emoji: reason names emoji rule" \
  "emoji" "$(printf '%s' "$result" | jq -r '.reason')"
assert_contains "Edit .md with heart emoji: reason shows offending line" \
  "❤" "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "docs/guide.md" "See this 😀 face")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with emoticon emoji: exits 2" "2" "$status"
assert_eq "Edit .md with emoticon emoji: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md with emoticon emoji: reason names emoji rule" \
  "emoji" "$(printf '%s' "$result" | jq -r '.reason')"

# ---------------------------------------------------------------------------
# Tests: Write on .md with emoji → block (exit 2)
# ---------------------------------------------------------------------------

input=$(make_write_input "docs/guide.md" "New content with 🎉 celebration")
result=$(run_hook "$input") ; status=$?
assert_eq "Write .md with emoji: exits 2" "2" "$status"
assert_eq "Write .md with emoji: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Write .md with emoji: reason names emoji rule" \
  "emoji" "$(printf '%s' "$result" | jq -r '.reason')"
assert_contains "Write .md with emoji: reason shows offending line" \
  "🎉" "$(printf '%s' "$result" | jq -r '.reason')"

# ---------------------------------------------------------------------------
# Tests: Edit on .md with no emoji → exit 0, no output
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "Hello world, no emoji here.")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md clean content: exits 0" "0" "$status"
assert_eq "Edit .md clean content: no output" "" "$result"

# ---------------------------------------------------------------------------
# Tests: Edit on non-.md path with emoji → exit 0, no output
# See .agents/prd-markdown.md: short-circuit for non-markdown paths
# ---------------------------------------------------------------------------

input=$(make_edit_input "main.go" "// See this ❤ symbol")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .go with emoji: exits 0" "0" "$status"
assert_eq "Edit .go with emoji: no output" "" "$result"

input=$(make_edit_input "config.yaml" "key: ❤ value")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .yaml with emoji: exits 0" "0" "$status"
assert_eq "Edit .yaml with emoji: no output" "" "$result"

# ---------------------------------------------------------------------------
# Tests: emoji outside the original five blocks — extended coverage
# These codepoints are commonly used by agents but outside the blocks
# named in the spec (clocks, media controls, arrows, stars).
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "⭐ Key feature")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with star emoji (U+2B50): exits 2" "2" "$status"
assert_eq "Edit .md with star emoji: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"

input=$(make_edit_input "README.md" "⌚ deadline")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with watch emoji (U+231A): exits 2" "2" "$status"

input=$(make_edit_input "README.md" "⏩ fast forward")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with fast-forward emoji (U+23E9): exits 2" "2" "$status"

input=$(make_edit_input "README.md" "⬅ go back")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with left-arrow emoji (U+2B05): exits 2" "2" "$status"

input=$(make_edit_input "README.md" "▶ play")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md with play-button emoji (U+25B6): exits 2" "2" "$status"

# ---------------------------------------------------------------------------
# Tests: empty or missing content field → exit 0
# See .agents/prd-markdown.md: hook input with missing or empty fields
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md empty new_string: exits 0" "0" "$status"

input=$(jq -n '{"tool_name":"Edit","tool_input":{"file_path":"README.md"}}')
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md missing new_string: exits 0" "0" "$status"

input=$(make_write_input "README.md" "")
result=$(run_hook "$input") ; status=$?
assert_eq "Write .md empty content: exits 0" "0" "$status"

input=$(jq -n '{"tool_name":"Write","tool_input":{"file_path":"README.md"}}')
result=$(run_hook "$input") ; status=$?
assert_eq "Write .md missing content: exits 0" "0" "$status"

# ---------------------------------------------------------------------------
# Tests: enumerated headings → block (exit 2)
# See .agents/prd-markdown.md
# Acceptance criteria from .agents/issues/02-block-enumerated-headings.md
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "# 1. Introduction")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '# 1. Introduction': exits 2" "2" "$status"
assert_eq "Edit .md '# 1. Introduction': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '# 1. Introduction': reason names enumerated-heading rule" \
  "enumerated heading" "$(printf '%s' "$result" | jq -r '.reason')"
assert_contains "Edit .md '# 1. Introduction': reason shows offending line" \
  "# 1. Introduction" "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "README.md" "## 2 Setup")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '## 2 Setup' (no period): exits 2" "2" "$status"
assert_eq "Edit .md '## 2 Setup': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '## 2 Setup': reason names enumerated-heading rule" \
  "enumerated heading" "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "README.md" "### 3. Step three")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '### 3. Step three': exits 2" "2" "$status"
assert_eq "Edit .md '### 3. Step three': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '### 3. Step three': reason names enumerated-heading rule" \
  "enumerated heading" "$(printf '%s' "$result" | jq -r '.reason')"

# Body line with digits must not trigger the rule.
input=$(make_edit_input "README.md" "Item 1. is the first item.")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md body line 'Item 1. is the first item.': exits 0" "0" "$status"
assert_eq "Edit .md body line with digits: no output" "" "$result"

# Headings whose text starts with a non-digit must not trigger the rule.
input=$(make_edit_input "README.md" "# Introduction")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '# Introduction' (non-digit heading): exits 0" "0" "$status"
assert_eq "Edit .md '# Introduction': no output" "" "$result"

input=$(make_edit_input "README.md" "## Step one")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '## Step one' (non-digit heading): exits 0" "0" "$status"
assert_eq "Edit .md '## Step one': no output" "" "$result"

# ---------------------------------------------------------------------------
# Tests: bold-only headings → block (exit 2)
# See .agents/prd-markdown.md
# Acceptance criteria from .agents/issues/03-block-bold-only-headings.md
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "# **Introduction**")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '# **Introduction**': exits 2" "2" "$status"
assert_eq "Edit .md '# **Introduction**': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '# **Introduction**': reason names bold-only-heading rule" \
  "bold-only heading" "$(printf '%s' "$result" | jq -r '.reason')"
assert_contains "Edit .md '# **Introduction**': reason shows offending line" \
  "# **Introduction**" "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "README.md" "## __Setup__")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '## __Setup__' (underscore bold): exits 2" "2" "$status"
assert_eq "Edit .md '## __Setup__': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '## __Setup__': reason names bold-only-heading rule" \
  "bold-only heading" "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "README.md" "### **Step three**  ")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '### **Step three**  ' (trailing whitespace): exits 2" "2" "$status"
assert_eq "Edit .md '### **Step three**  ': decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md '### **Step three**  ': reason names bold-only-heading rule" \
  "bold-only heading" "$(printf '%s' "$result" | jq -r '.reason')"

# Heading with inline bold (partial) must not trigger the rule.
input=$(make_edit_input "README.md" "## Step **two** is hard")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '## Step **two** is hard' (partial bold): exits 0" "0" "$status"
assert_eq "Edit .md '## Step **two** is hard': no output" "" "$result"

# Non-heading line wrapped in bold must not trigger the rule.
input=$(make_edit_input "README.md" "**Important**")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '**Important**' (non-heading bold): exits 0" "0" "$status"
assert_eq "Edit .md '**Important**': no output" "" "$result"

# Heading with multiple bold spans (plain text between them) must not trigger.
# The greedy '.*' would wrongly match from the first '**' to the last '**',
# treating the plain text between spans as if the whole heading were wrapped.
input=$(make_edit_input "README.md" "# **one** and **two**")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '# **one** and **two**' (multiple bold spans): exits 0" "0" "$status"
assert_eq "Edit .md '# **one** and **two**': no output" "" "$result"

# Underscore variant of the same false-positive case.
input=$(make_edit_input "README.md" "## __one__ and __two__")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md '## __one__ and __two__' (multiple underscore spans): exits 0" "0" "$status"
assert_eq "Edit .md '## __one__ and __two__': no output" "" "$result"

# ---------------------------------------------------------------------------
# Tests: unescaped dollar sign → block (exit 2)
# See .agents/prd-markdown.md and .agents/issues/04-block-unescaped-dollar-signs.md
# ---------------------------------------------------------------------------

input=$(make_edit_input "README.md" "The cost is \$5")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md 'The cost is \$5' (unescaped dollar): exits 2" "2" "$status"
assert_eq "Edit .md unescaped dollar: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md unescaped dollar: reason names unescaped dollar sign rule" \
  "unescaped dollar sign" "$(printf '%s' "$result" | jq -r '.reason')"
assert_contains "Edit .md unescaped dollar: reason shows offending line" \
  '$5' "$(printf '%s' "$result" | jq -r '.reason')"

input=$(make_edit_input "README.md" 'The cost is \$5')
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md 'The cost is \\\$5' (escaped dollar): exits 0" "0" "$status"
assert_eq "Edit .md escaped dollar: no output" "" "$result"

# \\$ means escaped backslash then bare dollar — must block.
# The single-char lookbehind (?<!\\)\$ incorrectly passes this because the
# char before $ is \; parity-aware matching is required.
# See .agents/issues/04-block-unescaped-dollar-signs.md
input=$(make_edit_input "README.md" 'The cost is \\$5')
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md 'The cost is \\\\\$5' (double-escaped backslash, bare dollar): exits 2" "2" "$status"
assert_eq "Edit .md double-escaped backslash dollar: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md double-escaped backslash dollar: reason names rule" \
  "unescaped dollar sign" "$(printf '%s' "$result" | jq -r '.reason')"

# Fenced code block: $ inside the fence is exempt.
input=$(make_edit_input "README.md" "$(printf '```bash\necho $HOME\n```')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md dollar inside fenced block: exits 0" "0" "$status"
assert_eq "Edit .md dollar inside fenced block: no output" "" "$result"

# $ outside a closed fence triggers the block.
input=$(make_edit_input "README.md" "$(printf '```bash\necho $HOME\n```\nThe price is $5')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md dollar outside closed fence: exits 2" "2" "$status"
assert_eq "Edit .md dollar outside closed fence: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md dollar outside closed fence: reason names unescaped dollar sign rule" \
  "unescaped dollar sign" "$(printf '%s' "$result" | jq -r '.reason')"

# False-positive case: entire new_string looks like a fenced block body.
# The hook has no context and treats $HOME as outside a fence.
# See .agents/issues/04-block-unescaped-dollar-signs.md (accepted trade-off).
input=$(make_edit_input "README.md" 'echo $HOME')
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md standalone 'echo \$HOME' (false-positive): exits 2" "2" "$status"
assert_eq "Edit .md standalone dollar: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"

# ---------------------------------------------------------------------------
# Tests: tilde fences — $ inside ~~~ block is exempt
# See .agents/prd-markdown.md
# ---------------------------------------------------------------------------

# Pure tilde fence: $ inside is exempt.
input=$(make_edit_input "README.md" "$(printf '~~~bash\necho $HOME\n~~~')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md dollar inside tilde-fenced block: exits 0" "0" "$status"
assert_eq "Edit .md dollar inside tilde-fenced block: no output" "" "$result"

# Dollar outside a closed tilde fence must be flagged.
input=$(make_edit_input "README.md" "$(printf '~~~bash\necho $HOME\n~~~\nThe price is $5')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md dollar outside closed tilde fence: exits 2" "2" "$status"
assert_eq "Edit .md dollar outside closed tilde fence: decision is block" \
  "block" "$(printf '%s' "$result" | jq -r '.decision')"
assert_contains "Edit .md dollar outside closed tilde fence: reason names rule" \
  "unescaped dollar sign" "$(printf '%s' "$result" | jq -r '.reason')"

# Backtick fence line inside a tilde fence must not close the tilde fence.
input=$(make_edit_input "README.md" "$(printf '~~~\necho $HOME\n```\nstill fenced $VAR\n~~~')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md backtick line inside tilde fence does not close it: exits 0" "0" "$status"
assert_eq "Edit .md backtick inside tilde fence: no output" "" "$result"

# Tilde fence line inside a backtick fence must not close the backtick fence.
input=$(make_edit_input "README.md" "$(printf '```\necho $HOME\n~~~\nstill fenced $VAR\n```')")
result=$(run_hook "$input") ; status=$?
assert_eq "Edit .md tilde line inside backtick fence does not close it: exits 0" "0" "$status"
assert_eq "Edit .md tilde inside backtick fence: no output" "" "$result"

# ---------------------------------------------------------------------------
# Tests: verbose-constructions soft rule → exit 0, additionalContext warning
# See .agents/prd-markdown.md
# ---------------------------------------------------------------------------

# "the fact that" must warn (not block).
input=$(make_edit_input "README.md" "The fact that this works is surprising")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'the fact that': exits 0" "0" "$status"
assert_contains "verbose-constructions 'the fact that': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'the fact that': additionalContext names phrase" \
  "the fact that" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'the fact that': additionalContext shows offending line" \
  "The fact that this works is surprising" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# "very" must warn (not block).
input=$(make_edit_input "README.md" "It is very fast")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'very': exits 0" "0" "$status"
assert_contains "verbose-constructions 'very': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'very': additionalContext names phrase" \
  "very" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# "owing to the fact that" must warn; full phrase takes priority over sub-phrase "the fact that".
input=$(make_edit_input "README.md" "Owing to the fact that we cached the result")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'owing to the fact that': exits 0" "0" "$status"
assert_contains "verbose-constructions 'owing to the fact that': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'owing to the fact that': additionalContext names phrase" \
  "owing to the fact that" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# "in many cases" must warn.
input=$(make_edit_input "README.md" "In many cases this is sufficient")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'in many cases': exits 0" "0" "$status"
assert_contains "verbose-constructions 'in many cases': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'in many cases': additionalContext names phrase" \
  "in many cases" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# "character of" must warn.
input=$(make_edit_input "README.md" "The character of the solution matters")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'character of': exits 0" "0" "$status"
assert_contains "verbose-constructions 'character of': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'character of': additionalContext names phrase" \
  "character of" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# "nature of" must warn.
input=$(make_edit_input "README.md" "The nature of the problem is complex")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'nature of': exits 0" "0" "$status"
assert_contains "verbose-constructions 'nature of': additionalContext names rule" \
  "verbose-constructions" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"
assert_contains "verbose-constructions 'nature of': additionalContext names phrase" \
  "nature of" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# Case-insensitive: "VERY" must warn.
input=$(make_edit_input "README.md" "This is VERY important")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions case-insensitive 'VERY': exits 0" "0" "$status"
assert_contains "verbose-constructions case-insensitive 'VERY': additionalContext names canonical phrase" \
  "very" "$(printf '%s' "$result" | jq -r '.hookSpecificOutput.additionalContext')"

# Clean content: no verbose construction, no warning output.
input=$(make_edit_input "README.md" "This is a clear and direct sentence.")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions clean content: exits 0" "0" "$status"
assert_eq "verbose-constructions clean content: no output" "" "$result"

# Word-boundary check: "every" must not match "very".
input=$(make_edit_input "README.md" "Not every solution works")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions 'every' does not match 'very': exits 0" "0" "$status"
assert_eq "verbose-constructions 'every' does not match 'very': no output" "" "$result"

# Verbose construction does not block — the edit is not rejected.
input=$(make_edit_input "README.md" "The fact that this works is surprising")
result=$(run_hook "$input") ; status=$?
assert_eq "verbose-constructions does not block (exit is 0, not 2):" "0" "$status"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]]
