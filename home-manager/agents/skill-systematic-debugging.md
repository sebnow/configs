---
name: systematic-debugging
description: |
  REQUIRED for ANY bug, error, test failure, or unexpected behavior.
  Enforces root cause investigation before fixes through 4-phase framework.
  Prevents random fix attempts and ensures evidence-based debugging.
  Auto-invokes on: test failures, errors, crashes, "not working", performance issues.
---

# Systematic Debugging

## Overview

Random fixes waste time and create new bugs. Quick patches mask underlying issues.

**Core principle:** ALWAYS find root cause before attempting fixes. Symptom fixes are failure.

**Violating the letter of this process is violating the spirit of debugging.**

## The Iron Law

```
NO FIXES WITHOUT ROOT CAUSE INVESTIGATION FIRST
```

If you haven't completed Phase 1, you cannot propose fixes.

## When to Use

Use for ANY technical issue:

- Test failures
- Bugs in production
- Unexpected behavior
- Performance problems
- Build failures
- Integration issues

**Use this ESPECIALLY when:**

- Under time pressure (emergencies make guessing tempting)
- "Just one quick fix" seems obvious
- You've already tried multiple fixes
- Previous fix didn't work
- You don't fully understand the issue

**Don't skip when:**

- Issue seems simple (simple bugs have root causes too)
- You're in a hurry (rushing guarantees rework)
- Manager wants it fixed NOW (systematic is faster than thrashing)

## The Four Phases

You MUST complete each phase before proceeding to the next.

## Phase Tracking (REQUIRED)

At the start of EACH phase, you MUST:

1. Use TodoWrite to create phase checkpoint
2. Mark current phase as "in_progress"
3. List specific tasks for this phase

Example:

```
- [in_progress] Phase 1: Root Cause Investigation - Reading error messages
- [pending] Phase 2: Pattern Analysis
- [pending] Phase 3: Hypothesis Testing
- [pending] Phase 4: Implementation
```

## Phase Transition Protocol

To move from one phase to the next, you MUST:

1. State: "I am completing Phase N because: [completion criteria met]"
2. Update TodoWrite: Mark phase N as completed
3. State: "I am beginning Phase N+1"
4. Update TodoWrite: Mark phase N+1 as in_progress

Do NOT silently move between phases. Each transition must be explicit.

## Tool Usage by Phase

**Phase 1-3 (Investigation):**

- ✓ Read, Grep, Glob, Bash (read-only commands)
- ✓ Edit/Write ONLY for debug statements (must be reverted)
- ❌ Edit/Write for fixes (FORBIDDEN)

**Phase 4 (Implementation):**

- ✓ Edit/Write for the ONE root cause fix
- ❌ Edit/Write for multiple changes (one at a time)

If you catch yourself using Edit/Write in Phase 1-3 for anything other than debug statements, STOP.

### Phase 1: Root Cause Investigation

**BEFORE attempting ANY fix, you MUST complete ALL steps below.**

**FORBIDDEN until Phase 1 complete:**

- ❌ Proposing any code changes
- ❌ Suggesting fixes or solutions
- ❌ Using Edit or Write tools for fixes
- ❌ Moving to Phase 2

**YOU MAY ONLY:**

- ✓ Add debug statements (temporary, will be removed)
- ✓ Read files and search code
- ✓ Run debugging tools
- ✓ Create temporary test files (will be deleted)

1. **Read Error Messages Carefully**
   - Don't skip past errors or warnings
   - They often contain the exact solution
   - Read stack traces completely
   - Note line numbers, file paths, error codes

2. **Reproduce Consistently**
   - Can you trigger it reliably?
   - What are the exact steps?
   - Does it happen every time?
   - If not reproducible → gather more data, don't guess

3. **Check Recent Changes**
   - What changed that could cause this?
   - Git diff, recent commits
   - New dependencies, config changes
   - Environmental differences

4. **Gather Evidence in Multi-Component Systems**

   **WHEN system has multiple components (CI → build → signing, API → service → database):**

   **BEFORE proposing fixes, add diagnostic instrumentation:**

   ```
   For EACH component boundary:
     - Log what data enters component
     - Log what data exits component
     - Verify environment/config propagation
     - Check state at each layer

   Run once to gather evidence showing WHERE it breaks
   THEN analyze evidence to identify failing component
   THEN investigate that specific component
   ```

   **Example: User reports "search returns no results"**

   System has multiple layers: Frontend → API → Search Service → Database

   Add instrumentation at EACH boundary:

   ```python
   import logging

   logger = logging.getLogger(__name__)

   # Layer 1: API endpoint receives request
   @app.route('/search')
   def search():
       query = request.args.get('q')
       logger.debug("[DEBUG:API:entry]", extra={"query": query})

       # Layer 2: Call search service
       logger.debug("[DEBUG:API:calling_service]", extra={"query": query})
       results = search_service.find(query)
       logger.debug("[DEBUG:API:service_returned]", extra={"result_count": len(results)})

       return jsonify(results)

   # Layer 3: Search service processes query
   def find(query):
       logger.debug("[DEBUG:SearchService:entry]", extra={"query": query})
       normalized = normalize_query(query)
       logger.debug("[DEBUG:SearchService:normalized]", extra={"normalized": normalized})

       # Layer 4: Database query
       logger.debug("[DEBUG:SearchService:calling_db]", extra={"query": normalized})
       rows = db.execute("SELECT * FROM items WHERE title LIKE ?", normalized)
       logger.debug("[DEBUG:SearchService:db_returned]", extra={"row_count": len(rows)})
       return rows
   ```

   **Running this reveals:**
   - API receives: `query="test"` ✓
   - Service receives: `query="test"` ✓
   - After normalization: `normalized=""` ✗ (Empty string!)
   - Database returns: 0 rows (because searching for empty string)

   **Root cause identified:** `normalize_query()` is incorrectly returning empty string.
   Now investigate ONLY that function, not the database or API layers.

5. **Trace Data Flow**

   **WHEN error is deep in call stack:**

   Use backward tracing to find the source:
   - Where does bad value originate?
   - What called this with bad value?
   - Keep tracing up until you find the source
   - Fix at source, not at symptom

   Add debug statements at each level of the call stack to trace the data flow.

#### Evidence Gathering Techniques

**Debug Statement Injection:**

Add debug statements with a consistent prefix for easy identification and cleanup.

Format: `[DEBUG:location:line] variable_values`

Examples:

```c
// C/C++
fprintf(stderr, "[DEBUG:UserManager::auth:142] user=\"%s\" id=%d result=%d\n", user, id, result);
```

```python
# Python
import logging
logger = logging.getLogger(__name__)
logger.debug("[DEBUG:auth_user:142]", extra={"user": user, "id": id, "result": result})
```

```go
// Go
import "log/slog"
slog.DebugContext(ctx, "[DEBUG:AuthUser:142]", "user", user, "id", id, "result", result)
```

All debug statements must include the "DEBUG:" prefix for easy cleanup.

**Language-Specific Debugging Tools:**

Use appropriate tools for the language:

- **Python**: `pdb` (debugger), `cProfile` (profiler), `tracemalloc` (memory), `pytest -vv` (verbose tests)
- **Go**: `delve` (debugger), `pprof` (profiler), `go test -race` (race detector), `go test -v` (verbose)
- **C/C++**: `gdb` or `lldb` (debuggers), `valgrind` (memory), `-fsanitize=address,undefined` (sanitizers), `perf` (profiler)
- **Rust**: `rust-gdb` or `rust-lldb`, `valgrind`, `cargo test -- --nocapture`
- **JavaScript/Node**: `node --inspect`, Chrome DevTools, `--trace-warnings`
- **System-level**: `strace` (Linux syscalls), `dtrace` (macOS/BSD), `tcpdump` (network), `lsof` (open files)

Verify tool availability before invoking.

**Investigation Strategies by Issue Type:**

_Memory Issues:_

- Log pointer values and dereferenced content
- Track allocations and deallocations
- Enable memory sanitizers (AddressSanitizer, Valgrind)
- Check for use-after-free, double-free, buffer overflows

_Concurrency Issues:_

- Log thread/goroutine/process IDs with state changes
- Track lock acquisition and release
- Enable race detectors (`-fsanitize=thread`, `go test -race`)
- Look for deadlocks, race conditions, ordering issues

_Performance Issues:_

- Add timing measurements around suspect code
- Use language-specific profilers before adding extensive debug statements
- Track memory allocations and garbage collection
- Identify hot paths and algorithmic bottlenecks

_State and Logic Issues:_

- Log state transitions with old and new values
- Break complex conditions into parts and log each evaluation
- Track variable changes through execution flow
- Verify input validation and boundary conditions

_Integration Issues:_

- Log all external interactions (API calls, database queries, file I/O)
- Verify configuration and connection parameters
- Check network connectivity and timeouts
- Test with minimal external dependencies

#### Phase 1 Completion Checklist

You have completed Phase 1 ONLY if you can answer ALL:

- □ **WHAT fails**: Exact component, line number, function name
- □ **WHEN it fails**: Reproducible steps or conditions
- □ **WHAT value is wrong**: Actual vs expected value
- □ **WHERE it originates**: Source of bad data/state (not just where it manifests)

If you cannot answer ALL four, you are STILL IN PHASE 1.

### Phase 2: Pattern Analysis

**Find the pattern before fixing:**

1. **Find Working Examples**
   - Locate similar working code in same codebase
   - What works that's similar to what's broken?

2. **Compare Against References**
   - If implementing pattern, read reference implementation COMPLETELY
   - Don't skim - read every line
   - Understand the pattern fully before applying

3. **Identify Differences**
   - What's different between working and broken?
   - List every difference, however small
   - Don't assume "that can't matter"

4. **Understand Dependencies**
   - What other components does this need?
   - What settings, config, environment?
   - What assumptions does it make?

### Phase 3: Hypothesis and Testing

**Scientific method:**

1. **Form Single Hypothesis**
   - State clearly: "I think X is the root cause because Y"
   - Write it down
   - Be specific, not vague

2. **Test Minimally**
   - Make the SMALLEST possible change to test hypothesis
   - One variable at a time
   - Don't fix multiple things at once

3. **Verify Before Continuing**
   - Did it work? Yes → Phase 4
   - Didn't work? Form NEW hypothesis
   - DON'T add more fixes on top

4. **When You Don't Know**
   - Say "I don't understand X"
   - Don't pretend to know
   - Ask for help
   - Research more

#### Evidence Requirements

Before forming a hypothesis, gather sufficient evidence:

- Add debug statements at key points in the execution path
- Run tests with multiple inputs, including edge cases
- Log entry and exit for suspect functions
- Create isolated test cases for reproduction

The goal is evidence-based investigation, not hitting arbitrary thresholds.
If you can form a well-supported hypothesis with less evidence, that's acceptable.
If you need more evidence, gather it.

#### When to Escalate

If after thorough investigation you cannot identify the root cause:

1. Document your investigation process
2. List hypotheses tested and evidence gathered
3. Identify what remains unclear
4. Escalate with your findings

Don't continue indefinitely.
If multiple hypotheses have been tested with evidence and the root cause remains elusive, escalation is appropriate.

## Before Proposing ANY Fix

Ask yourself these questions:

1. Have I completed Phase 1? (Can I answer all 4 checklist items?)
2. Have I found a working counterexample? (Phase 2)
3. Have I stated ONE specific hypothesis? (Phase 3)
4. Have I tested that hypothesis minimally? (Phase 3)

If ANY answer is "no", you are NOT ready to fix. State which phase you're in and continue investigation.

### Phase 4: Implementation

**Fix the root cause, not the symptom:**

1. **Create Failing Test Case**
   - Simplest possible reproduction
   - Automated test if possible
   - One-off test script if no framework
   - MUST have before fixing
   - Write a test that fails with current code, will pass after fix

2. **Implement Single Fix**
   - Address the root cause identified
   - ONE change at a time
   - No "while I'm here" improvements
   - No bundled refactoring

3. **Clean Up Investigation**

   Before marking Phase 4 complete, you MUST:
   1. Run: `grep -r "\[DEBUG:" .` → Result must be empty
   2. Check TodoWrite for any "debug", "temporary", "test_debug" items
   3. Run: `find . -name "test_debug_*"` → Result must be empty
   4. Confirm: Only the ONE root cause fix remains in the codebase

   If any check fails, you have not completed cleanup.

4. **Verify Fix**
   - Test passes now?
   - No other tests broken?
   - Issue actually resolved?

5. **If Fix Doesn't Work**
   - STOP
   - Count: How many fixes have you tried?
   - If < 3: Return to Phase 1, re-analyze with new information
   - **If ≥ 3: STOP and question the architecture (step 6 below)**
   - DON'T attempt Fix #4 without architectural discussion

6. **If 3+ Fixes Failed: Question Architecture**

   **Pattern indicating architectural problem:**
   - Each fix reveals new shared state/coupling/problem in different place
   - Fixes require "massive refactoring" to implement
   - Each fix creates new symptoms elsewhere

   **STOP and question fundamentals:**
   - Is this pattern fundamentally sound?
   - Are we "sticking with it through sheer inertia"?
   - Should we refactor architecture vs. continue fixing symptoms?

   **Discuss with your human partner before attempting more fixes**

   This is NOT a failed hypothesis - this is a wrong architecture.

## Red Flags - STOP and Follow Process

If you catch yourself thinking:

- "Quick fix for now, investigate later"
- "Just try changing X and see if it works"
- "Add multiple changes, run tests"
- "Skip the test, I'll manually verify"
- "It's probably X, let me fix that"
- "I don't fully understand but this might work"
- "Pattern says X but I'll adapt it differently"
- "Here are the main problems: [lists fixes without investigation]"
- Proposing solutions before tracing data flow
- **"One more fix attempt" (when already tried 2+)**
- **Each fix reveals new problem in different place**

**ALL of these mean: STOP. Return to Phase 1.**

**If 3+ fixes failed:** Question the architecture (see Phase 4.6)

**After investigation:** Always clean up debug statements and temporary test files (see Phase 4.3)

## LLM Anti-Pattern Detection

If you output ANY of these phrases, you are violating this skill:

- "Let's try..." → STOP. Have you completed Phase 1?
- "We could fix this by..." → STOP. Are you in Phase 4?
- "One approach would be..." → STOP. Have you formed ONE hypothesis?
- "I think the issue might be..." → STOP. Where's your evidence?
- "Let's make a few changes..." → STOP. ONE change at a time.
- "This should work..." → STOP. Have you tested your hypothesis?
- "I'll implement..." → STOP. Have you answered the 4 Phase 1 checklist items?

When you detect these phrases in your own output, immediately:

1. Stop and acknowledge the violation
2. State which phase you should be in
3. Return to that phase and continue properly

## your human partner's Signals You're Doing It Wrong

**Watch for these redirections:**

- "Is that not happening?" - You assumed without verifying
- "Will it show us...?" - You should have added evidence gathering
- "Stop guessing" - You're proposing fixes without understanding
- "Ultrathink this" - Question fundamentals, not just symptoms
- "We're stuck?" (frustrated) - Your approach isn't working

**When you see these:** STOP. Return to Phase 1.

## Common Rationalizations

| Excuse                                       | Reality                                                                 |
| -------------------------------------------- | ----------------------------------------------------------------------- |
| "Issue is simple, don't need process"        | Simple issues have root causes too. Process is fast for simple bugs.    |
| "Emergency, no time for process"             | Systematic debugging is FASTER than guess-and-check thrashing.          |
| "Just try this first, then investigate"      | First fix sets the pattern. Do it right from the start.                 |
| "I'll write test after confirming fix works" | Untested fixes don't stick. Test first proves it.                       |
| "Multiple fixes at once saves time"          | Can't isolate what worked. Causes new bugs.                             |
| "Reference too long, I'll adapt the pattern" | Partial understanding guarantees bugs. Read it completely.              |
| "I see the problem, let me fix it"           | Seeing symptoms ≠ understanding root cause.                             |
| "One more fix attempt" (after 2+ failures)   | 3+ failures = architectural problem. Question pattern, don't fix again. |

## Quick Reference

| Phase                 | Key Activities                                                    | Success Criteria            |
| --------------------- | ----------------------------------------------------------------- | --------------------------- |
| **1. Root Cause**     | Read errors, reproduce, check changes, gather evidence with tools | Understand WHAT and WHY     |
| **2. Pattern**        | Find working examples, compare                                    | Identify differences        |
| **3. Hypothesis**     | Form theory, test minimally, escalate if needed                   | Confirmed or new hypothesis |
| **4. Implementation** | Create test, fix, clean up debug code, verify                     | Bug resolved, tests pass    |

## When Process Reveals "No Root Cause"

If systematic investigation reveals issue is truly environmental, timing-dependent, or external:

1. You've completed the process
2. Document what you investigated
3. Implement appropriate handling (retry, timeout, error message)
4. Add monitoring/logging for future investigation

**But:** 95% of "no root cause" cases are incomplete investigation.

## Integration with Other Skills

**Complementary skills:**

- **defense-in-depth** - Add validation at multiple layers after finding root cause
- **condition-based-waiting** - Replace arbitrary timeouts identified in Phase 2
- **verification-before-completion** - Verify fix worked before claiming success

## Real-World Impact

From debugging sessions:

- Systematic approach: 15-30 minutes to fix
- Random fixes approach: 2-3 hours of thrashing
- First-time fix rate: 95% vs 40%
- New bugs introduced: Near zero vs common
