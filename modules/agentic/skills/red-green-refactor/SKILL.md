---
name: red-green-refactor
description: "Domain-agnostic TDD methodology for iterative improvement. Enforces baseline measurement, minimal changes, and rigorous testing through commitment devices. Use when applying red-green-refactor workflow to any domain: prompts, skills, code, tests. Triggers: TDD methodology, baseline measurement, iterative improvement, red-green-refactor."
---

# Red-Green-Refactor Methodology

Test-Driven Development applies beyond code:
observe failures with current approach,
make minimal improvements,
then test rigorously.

This skill defines the domain-agnostic methodology.
Consuming skills define what "baseline", "minimal change", and "testing" mean
in their domain.

## Red Phase: Establish Baseline

Before changing anything, measure current performance.

State: "Beginning Red phase - establishing baseline"

Required steps:

1. Define success criteria:
   - What does good output look like?
   - How will you measure it?
   - What's the acceptable threshold?

2. Observe and measure baseline:
   - Run current approach against criteria
   - Document specific failures and patterns
   - Record baseline metrics

3. Document failures:
   - What patterns appear in failures?
   - What rationalizations or mistakes occur?
   - What concrete symptoms indicate the problem?

Phase gate: You cannot proceed to Green phase
without documented baseline measurements.

Forbidden rationalizations:

- "This is simple enough to skip testing"
- "I know what the output will be"
- "I'll improve it then test later"
- "Quick changes are better than none"

If you haven't measured baseline, you don't know if changes help.

Failure mode:
Skipping Red creates changes that may perform worse than the original.

## Green Phase: Minimal Improvements

Apply one change at a time. Measure after each change.

State: "Beginning Green phase - making minimal improvements"

Required steps:

1. Address one observed failure at a time
2. Make the smallest change that could fix it
3. Measure impact against baseline after each change
4. Only proceed to next failure when current one is addressed

Do not change multiple things simultaneously.
This isolates what helps vs what hurts.

Phase gate: Each change must show measurable improvement
or be reverted before trying alternatives.

## Refactor Phase: Rigorous Testing

Test comprehensively. Compare against baseline. Close loopholes.

State: "Beginning Refactor phase - testing rigorously"

Required steps:

1. Run against all test cases, not just the ones you fixed
2. Compare results against original baseline
3. Verify no regressions from Green phase changes
4. Close loopholes discovered during testing

Common loopholes:

- Required steps get skipped
- Rationalizations bypass the process
- Edge cases missed in initial testing
- Changes that help one case hurt another

Only after rigorous testing can you claim improvement.

## Commitment Devices

These mechanisms prevent process shortcuts:

**State announcements** -
Declare current phase before starting work.
This creates accountability.

**Phase gates** -
Cannot proceed to next phase without completing current one.
Red requires baseline. Green requires measured improvement.

**Forbidden rationalizations** -
Explicit list of thoughts that signal process violation.
When you think these, stop and follow the methodology.

**Failure mode warnings** -
Each phase documents what goes wrong when skipped,
making the cost of shortcuts concrete.

## Domain Specialization

Consuming skills define domain-specific meanings:

- **Baseline**: What to measure
  (test results, agent behavior, prompt output, code health)
- **Minimal change**: What counts as one change
  (one technique, one instruction, one code fix)
- **Testing**: How to verify improvement
  (test cases, multi-run statistics, fresh instances, test suites)
- **Loopholes**: Domain-specific failure patterns to watch for

The methodology stays the same. The domain provides the vocabulary.
