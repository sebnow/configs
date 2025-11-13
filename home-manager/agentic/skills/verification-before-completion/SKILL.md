---
# https://github.com/obra/superpowers/blob/main/skills/verification-before-completion/SKILL.md
name: verification-before-completion
description: |
  Invoke proactively when about to claim work is complete,
  fixed,
  or passing,
  before committing
  or creating PRs - requires running verification commands and confirming output before making any success claims;
  evidence before assertions always
---

# Verification Before Completion

## Overview

Claiming work is complete without verification is dishonesty, not efficiency.

Core principle: Evidence before claims, always.

## Verification Protocol

Before claiming any status:

1. Identify: What command proves this claim?
2. Run: Execute the full command (fresh, complete)
3. Read: Full output, check exit code, count failures
4. Verify: Does output confirm the claim?
   - If no: State actual status with evidence
   - If yes: State claim with evidence
5. Only then: Make the claim

Skip any step = no claim permitted.

## Common Failures

| Claim | Requires | Not Sufficient |
|-------|----------|----------------|
| Tests pass | Test output: 0 failures | Previous run, "should pass" |
| Linter clean | Linter output: 0 errors | Partial check |
| Build succeeds | Build: exit 0 | Linter passing |
| Bug fixed | Test symptom: passes | Code changed |
| Regression test | Red-green cycle | Test passes once |
| Agent completed | VCS diff verified | Agent reports success |
| Requirements met | Line-by-line check | Tests passing |

## Red Flags

Stop immediately if:

- Using "should", "probably", "seems to"
- Expressing satisfaction before verification ("Great!", "Perfect!", "Done!")
- About to commit/push/PR without verification
- Trusting agent success reports
- Relying on partial verification
- Any wording implying success without having run verification

## Verification Patterns

Tests:
```
Good: [Run test command] [See: 34/34 pass] "All tests pass"
Bad: "Should pass now" / "Looks correct"
```

Regression tests (TDD Red-Green):
```
Good: Write -> Run (pass) -> Revert fix -> Run (must fail) -> Restore -> Run (pass)
Bad: "I've written a regression test" (without red-green verification)
```

Build:
```
Good: [Run build] [See: exit 0] "Build passes"
Bad: "Linter passed" (linter doesn't verify compilation)
```

Requirements:
```
Good: Re-read plan -> Create checklist -> Verify each -> Report
Bad: "Tests pass, phase complete"
```

Agent delegation:
```
Good: Agent reports success -> Check VCS diff -> Verify -> Report actual state
Bad: Trust agent report
```

## Rationalization Prevention

No shortcuts exist. Counter every excuse:

- "Should work now" -> Run verification
- "I'm confident" -> Confidence is not evidence
- "Just this once" -> No exceptions
- "Linter passed" -> Linter is not compiler
- "Agent said success" -> Verify independently
- "Partial check is enough" -> Partial proves nothing

## When To Apply

Always before:
- Any variation of success/completion claims
- Any expression of satisfaction
- Any positive statement about work state
- Committing, PR creation, task completion
- Moving to next task

Rule applies to exact phrases, paraphrases, synonyms, and implications.

## Bottom Line

Run the command. Read the output. Then claim the result.

No exceptions.
