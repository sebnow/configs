---
name: systematic-debugging
description: |
  Required for any bug, error, test failure, or unexpected behavior.
  Enforces root cause investigation before fixes through 4-phase framework.
  Prevents random fix attempts and ensures evidence-based debugging.
  Auto-invokes on: test failures, errors, crashes, "not working", performance issues.
---

# Systematic Debugging

## Overview

Random fixes waste time and create new bugs. Quick patches mask underlying issues.

Core principle: Always find root cause before attempting fixes. Symptom fixes are failure.

No fixes without root cause investigation first. If you haven't completed Phase 1, you cannot propose fixes.

## When to Use

For any technical issue: test failures, bugs, unexpected behavior, performance problems, build failures, integration issues

Especially when: Under time pressure, "quick fix" seems obvious, already tried multiple fixes, previous fix didn't work, don't fully understand issue

Never skip when: Issue seems simple, you're in a hurry, under pressure

## Phase Tracking (Required)

At start of each phase:
1. Use TodoWrite to create phase checkpoint
2. Mark current phase as "in_progress"
3. List specific tasks

## Phase Transition Protocol

To move between phases:
1. State: "I am completing Phase N because: [completion criteria met]"
2. Update TodoWrite: Mark phase N as completed
3. State: "I am beginning Phase N+1"
4. Update TodoWrite: Mark phase N+1 as in_progress

Do not silently move between phases. Each transition must be explicit.

## Tool Usage by Phase

Phase 1-3 (Investigation):
- Allowed: Read, Grep, Glob, Bash (read-only commands)
- Allowed: Edit/Write only for debug statements (must be reverted)
- Forbidden: Edit/Write for fixes

Phase 4 (Implementation):
- Allowed: Edit/Write for the one root cause fix
- Forbidden: Edit/Write for multiple changes (one at a time)

If you catch yourself using Edit/Write in Phase 1-3 for anything other than debug statements, stop.

### Phase 1: Root Cause Investigation

Before attempting any fix, you must complete all steps below.

Forbidden until Phase 1 complete:
- Proposing any code changes
- Suggesting fixes or solutions
- Using Edit or Write tools for fixes
- Moving to Phase 2

You may only:
- Add debug statements (temporary, will be removed)
- Read files and search code
- Run debugging tools
- Create temporary test files (will be deleted)

Steps:

1. Read Error Messages Carefully: Don't skip errors/warnings, read stack traces completely, note line numbers/paths/codes

2. Reproduce Consistently: Can you trigger it reliably? Exact steps? Every time? If not reproducible, gather more data

3. Check Recent Changes: What changed that could cause this? Git diff, recent commits, dependencies, config, environment

4. Gather Evidence in Multi-Component Systems

When system has multiple components (API -> service -> database):

Add diagnostic instrumentation at each boundary:
- Log data entering component
- Log data exiting component
- Verify environment/config propagation
- Check state at each layer

Run once to gather evidence showing where it breaks, then analyze to identify failing component, then investigate that specific component only.

5. Trace Data Flow

When error is deep in call stack:
- Where does bad value originate?
- What called this with bad value?
- Keep tracing up until you find the source
- Fix at source, not at symptom

Add debug statements at each level of the call stack.

#### Evidence Gathering Techniques

Debug Statement Injection:

Format: `[DEBUG:location:line] variable_values`

Examples:
```c
fprintf(stderr, "[DEBUG:UserManager::auth:142] user=\"%s\" id=%d result=%d\n", user, id, result);
```

```python
import logging
logger = logging.getLogger(__name__)
logger.debug("[DEBUG:auth_user:142]", extra={"user": user, "id": id, "result": result})
```

```go
import "log/slog"
slog.DebugContext(ctx, "[DEBUG:AuthUser:142]", "user", user, "id", id, "result", result)
```

All debug statements must include "DEBUG:" prefix for easy cleanup.

Language-Specific Debugging Tools:

- Python: pdb, cProfile, tracemalloc, pytest -vv
- Go: delve, pprof, go test -race, go test -v
- C/C++: gdb, lldb, valgrind, -fsanitize=address/undefined, perf
- Rust: rust-gdb, rust-lldb, valgrind, cargo test --nocapture
- JavaScript/Node: node --inspect, Chrome DevTools, --trace-warnings
- System: strace (Linux), dtrace (macOS/BSD), tcpdump, lsof

Verify tool availability before invoking.

Investigation Strategies by Issue Type:

- Memory: Log pointers/content, track allocations, enable sanitizers (AddressSanitizer, Valgrind), check use-after-free/double-free/overflows
- Concurrency: Log thread/goroutine IDs with state, track locks, enable race detectors, look for deadlocks/races/ordering
- Performance: Time suspect code, use profilers before extensive debug statements, track allocations/GC, identify hot paths
- State/Logic: Log state transitions with old/new values, break complex conditions into parts, track variable changes, verify input validation
- Integration: Log external interactions (API/database/file I/O), verify config/connections, check network/timeouts, test with minimal dependencies

#### Phase 1 Completion Checklist

You have completed Phase 1 only if you can answer all:

- [ ] What fails: Exact component, line number, function name
- [ ] When it fails: Reproducible steps or conditions
- [ ] What value is wrong: Actual vs expected value
- [ ] Where it originates: Source of bad data/state (not just where it manifests)

If you cannot answer all four, you are still in Phase 1.

### Phase 2: Pattern Analysis

Find the pattern before fixing:

1. Find Working Examples: Locate similar working code in same codebase
2. Compare Against References: If implementing pattern, read reference implementation completely
3. Identify Differences: What's different between working and broken? List every difference
4. Understand Dependencies: What other components, settings, config, environment does this need?

### Phase 3: Hypothesis and Testing

Scientific method:

1. Form Single Hypothesis: State clearly "I think X is root cause because Y" - be specific
2. Test Minimally: Make smallest possible change to test hypothesis, one variable at a time
3. Verify Before Continuing: Did it work? Yes -> Phase 4. No -> Form new hypothesis, don't add more fixes
4. When You Don't Know: Say "I don't understand X", don't pretend to know, ask for help, research

#### Evidence Requirements

Before forming hypothesis, gather sufficient evidence:
- Add debug statements at key points
- Run tests with multiple inputs, edge cases
- Log entry/exit for suspect functions
- Create isolated test cases

Goal is evidence-based investigation, not arbitrary thresholds. If you can form well-supported hypothesis with less evidence, acceptable. If you need more, gather it.

#### When to Escalate

If after thorough investigation you cannot identify root cause:
1. Document your investigation process
2. List hypotheses tested and evidence gathered
3. Identify what remains unclear
4. Escalate with your findings

Don't continue indefinitely. If multiple hypotheses tested with evidence and root cause remains elusive, escalation is appropriate.

## Before Proposing Any Fix

Ask yourself:
1. Have I completed Phase 1? (Can I answer all 4 checklist items?)
2. Have I found a working counterexample? (Phase 2)
3. Have I stated one specific hypothesis? (Phase 3)
4. Have I tested that hypothesis minimally? (Phase 3)

If any answer is "no", you are not ready to fix. State which phase you're in and continue investigation.

### Phase 4: Implementation

Fix root cause, not symptom:

1. Create Failing Test Case: Simplest possible reproduction, automated test if possible, one-off script if no framework, must have before fixing. Write test that fails with current code, will pass after fix.

2. Implement Single Fix: Address root cause identified, one change at a time, no "while I'm here" improvements, no bundled refactoring

3. Clean Up Investigation

Before marking Phase 4 complete:
- Run: `grep -r "\[DEBUG:" .` -> Result must be empty
- Check TodoWrite for any "debug", "temporary", "test_debug" items
- Run: `find . -name "test_debug_*"` -> Result must be empty
- Confirm: Only the one root cause fix remains in codebase

If any check fails, you have not completed cleanup.

4. Verify Fix: Test passes now? No other tests broken? Issue actually resolved?

5. If Fix Doesn't Work
- Stop
- Count: How many fixes have you tried?
- If < 3: Return to Phase 1, re-analyze with new information
- If >= 3: Stop and question the architecture (step 6)
- Don't attempt Fix #4 without architectural discussion

6. If 3+ Fixes Failed: Question Architecture

Pattern indicating architectural problem:
- Each fix reveals new shared state/coupling/problem elsewhere
- Fixes require "massive refactoring" to implement
- Each fix creates new symptoms elsewhere

Stop and question fundamentals:
- Is this pattern fundamentally sound?
- Are we "sticking with it through sheer inertia"?
- Should we refactor architecture vs continue fixing symptoms?

Discuss with user before attempting more fixes. This is not failed hypothesis, this is wrong architecture.

## Red Flags

Stop immediately if you catch yourself thinking:

- "Quick fix for now, investigate later"
- "Just try changing X and see if it works"
- "Add multiple changes, run tests"
- "Skip the test, I'll manually verify"
- "It's probably X, let me fix that"
- "I don't fully understand but this might work"
- "Pattern says X but I'll adapt it differently"
- "Here are the main problems: [lists fixes without investigation]"
- Proposing solutions before tracing data flow
- "One more fix attempt" (when already tried 2+)
- Each fix reveals new problem in different place

All of these mean: Stop. Return to Phase 1.

If 3+ fixes failed: Question the architecture (see Phase 4.6)
After investigation: Always clean up debug statements and temporary test files (see Phase 4.3)

## LLM Anti-Pattern Detection

If you output any of these phrases, you are violating this skill:

- "Let's try..." -> Stop. Have you completed Phase 1?
- "We could fix this by..." -> Stop. Are you in Phase 4?
- "One approach would be..." -> Stop. Have you formed one hypothesis?
- "I think the issue might be..." -> Stop. Where's your evidence?
- "Let's make a few changes..." -> Stop. One change at a time.
- "This should work..." -> Stop. Have you tested your hypothesis?
- "I'll implement..." -> Stop. Have you answered the 4 Phase 1 checklist items?

When you detect these phrases in your own output, immediately:
1. Stop and acknowledge the violation
2. State which phase you should be in
3. Return to that phase and continue properly

## User Signals You're Doing It Wrong

Watch for these redirections:
- "Is that not happening?" - You assumed without verifying
- "Will it show us...?" - You should have added evidence gathering
- "Stop guessing" - You're proposing fixes without understanding
- "Ultrathink this" - Question fundamentals, not just symptoms
- "We're stuck?" (frustrated) - Your approach isn't working

When you see these: Stop. Return to Phase 1.

## Common Rationalizations

Counter every excuse:

- "Issue is simple, don't need process" -> Simple issues have root causes too
- "Emergency, no time for process" -> Systematic is faster than thrashing
- "Just try this first, then investigate" -> First fix sets the pattern
- "I'll write test after confirming fix works" -> Untested fixes don't stick
- "Multiple fixes at once saves time" -> Can't isolate what worked
- "Reference too long, I'll adapt the pattern" -> Partial understanding guarantees bugs
- "I see the problem, let me fix it" -> Seeing symptoms != understanding root cause
- "One more fix attempt" (after 2+ failures) -> 3+ failures = architectural problem

## When Process Reveals "No Root Cause"

If systematic investigation reveals issue is truly environmental, timing-dependent, or external:
1. You've completed the process
2. Document what you investigated
3. Implement appropriate handling (retry, timeout, error message)
4. Add monitoring/logging for future investigation

But: 95% of "no root cause" cases are incomplete investigation.

## Integration with Other Skills

Complementary skills:
- defense-in-depth: Add validation at multiple layers after finding root cause
- condition-based-waiting: Replace arbitrary timeouts identified in Phase 2
- verification-before-completion: Verify fix worked before claiming success
