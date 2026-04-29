# Pressure Testing

Pressure testing measures a prompt change with parallel fresh-agent runs.
It is the operational form of "run multiple times per test case" from the
Refactor phase.

## When to use

- Validating that a new rule actually changes agent behavior
- Comparing two phrasings of the same rule
- Checking whether an edit regressed previously-passing scenarios
- Re-testing a claim before relying on it

## Method

For each scenario:

1. Define the scenario as a single self-contained prompt with a
   verifiable pass/fail criterion (e.g. "agent uses API X", "agent
   picks list A", "agent flags the issue").
2. Spawn N fresh subagents with the SAME prompt in parallel. Each agent
   must start from a clean context — no conversation history, no
   carry-over reasoning, only the prompt under test.
3. Record per-run pass/fail. Tally per scenario.
4. Compare to a control run with no rule, to isolate the rule's effect.

## Sample size

n=5 is a reasonable starting point for clear signal. n=3 with unanimous
outcomes also reads cleanly; mixed outcomes at n=3 require running more.
For high-variance scenarios (long prompts, stochastic outputs, ambiguous
criteria), increase N until the signal stabilizes.

n is not a target. It is the number you need to be confident the result
is the rule, not noise.

## What unanimity means

Unanimous pass/fail across N runs is a strong signal that the rule, as
written, drives the outcome. It does not prove the rule is the cause —
the prompt may have other cues that align with the rule. Compare against
a control (no rule) to isolate the rule's contribution.

A scenario where the control already passes is not testing the rule.
The model already does the right thing without it. Pick a different
scenario.

## Baseline first

Before testing two phrasings of a rule, run the no-rule control:

- Control n=3 → measures the model's default
- Phrasing A n=3 → measures whether A overrides the default
- Phrasing B n=3 → measures whether B overrides the default

Without the control, you cannot tell whether the rule helped or the
model would have done the right thing anyway.

## Worked example

Suppose someone claims: "Descriptive rule phrasing fails to change agent
behavior; only enforcement language ('Use X. Never use Y.') succeeds."
Pressure-test before using this as a guide.

Pick a rule the model has a known default against. Example: Python
agents tend to write `lru_cache(maxsize=None)` by default; the modern
form is `functools.cache`.

Three groups of three fresh subagents, each asked to write
`fib(n: int) -> int` memoized:

- Control (no rule) — 3/3 wrote `lru_cache(maxsize=None)`. Default
  confirmed; the scenario is testing something.
- Descriptive phrasing ("Some teams prefer `cache` for unbounded
  caches.") — 3/3 wrote `cache`.
- Enforcement phrasing ("Use `cache`. Never use `lru_cache(maxsize=
  None)`.") — 3/3 wrote `cache`.

Result: descriptive phrasing flipped the default just as reliably as
enforcement. The "descriptive fails" claim does not survive this
re-test on this rule. It would be wrong to treat it as a universal
principle on the strength of a single prior run.

The methodological point: a claim with concrete numbers attached can
still fail to replicate. Run the control plus comparison yourself
before treating someone else's result as a guide.

## Common loopholes

- Running the test inside the parent's context. The parent's loaded
  skills, conversation history, and prior reasoning leak into the test.
  Use fresh subagents only.
- Choosing a scenario where the control already passes. Measures
  nothing. The rule under test is being held up by something else
  (often: another active skill that already covers the same rule).
- Skipping the control. Without it you cannot attribute the result.
- Treating non-unanimous results as noise. They are signal that the
  rule is partially load-bearing. Investigate the failing run.
- Encoding sample sizes or compliance deltas as constants in guidance.
  They are evidence points from one run, not invariants.
