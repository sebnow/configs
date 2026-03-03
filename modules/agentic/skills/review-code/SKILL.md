---
name: review-code
description: "Performs structured, multi-perspective code review. Selects review perspectives based on the changeset, then produces a severity-ranked report with actionable findings. Use when reviewing code changes, verifying code safety/correctness, pre-merge reviews, or auditing existing code. Triggers: 'review code', 'code review', 'review changes', 'review this PR', 'review these commits'. Do NOT use for style-only feedback or cosmetic nitpicks."
---

# Code Review

A read-only review process.
Produces a report. Does not modify code.
The report is input for downstream planning and implementation.

Use available tools to support the review:
linters, static analyzers, type checkers, test suites,
debuggers, or any other diagnostic tool
available in the project environment.

## Instructions

### Step: Gather Changes

Obtain the code to review based on user input:
a diff, a set of commits, specific files, or a directory.

For "unpublished changes", "branch changes", or similar,
use the project's source control tool.

If the scope is ambiguous, ask.

### Step: Select Perspectives

Analyze the changeset and select review perspectives.
State the selected perspectives with a brief rationale.

Always apply these perspectives:

- **Security**: Credentials in code/logs, injection vectors,
  missing auth checks, input validation gaps, data exposure.
- **Performance**: Unbounded growth, missing backpressure,
  blocking in hot paths, algorithmic complexity,
  unnecessary allocations,
  per-item work that could be batched.
- **Documentation**: Outdated or incorrect comments/docs,
  missing docs for non-obvious decisions, API doc gaps.

Activate these when the changeset touches relevant code:

- **Concurrency**: Unsynchronized shared mutable state,
  race conditions, deadlocks, task/thread leaks.
- **Error handling**: Missing handling on failure paths,
  lost error information, implementation details
  leaking across abstraction boundaries.
- **API compatibility**: Breaking changes to public interfaces,
  missing versioning, backward compatibility.
- **Domain alignment**: Code structure vs domain boundaries,
  business logic correctness, domain terminology.
- **Resource management**: Leaks (connections, handles, memory),
  missing cleanup on error paths, unterminable background tasks.
- **Observability**: Missing logging/tracing for failure scenarios,
  monitoring gaps.
- **Adversarial**: What assumptions could be wrong?
  What happens when dependencies fail?
  What invariants are unenforced?
  What edge cases are silently ignored?

Add ad-hoc perspectives if warranted.
When in doubt, activate — missing a real issue
is worse than an empty perspective.

Run each perspective in its own agent instance
so it can focus without distraction from other perspectives.
For each perspective,
spawn an agent via the shell
with a prompt containing the perspective's focus area
and the changeset.
Perspectives can run in parallel when possible.
Each invocation should output findings
in the per-finding format from the Report Format section.
Collect all outputs and merge them into the final report.

If spawning sub-agents fails or is unavailable,
fall back to reviewing each perspective sequentially.

### Step: Review Each Perspective

For each selected perspective, critically examine the code.
Evaluate whether practices are appropriate in context;
do not apply rules dogmatically.

For each issue, record:
perspective, location (file:line or region),
severity, description, consequence, and recommendation.

Severity definitions:

- **High**: Production failures, data loss,
  security breaches, crashes, corruption.
- **Medium**: Degraded operation, correctness issues,
  maintainability risks, silent failures.
- **Low**: Minor improvements with real but limited impact.

Skip cosmetic, subjective, or zero-impact findings.
Do not flag DRY/YAGNI/etc. violations
unless there is a concrete negative consequence.

Check project documentation for established conventions,
error handling patterns, performance requirements,
architecture decisions, and domain terminology.

### Step: Write Report

Output the report in this structure:

```
## Perspectives

- **[Name]** (always/conditional): [rationale]

## Summary

[2-3 sentences: what was reviewed, overall assessment.]

| Severity | Count |
|----------|-------|
| High     | N     |
| Medium   | N     |
| Low      | N     |

## High

### [Perspective]: [Title]

**Location:** `path/to/file:line`
**Description:** [What is wrong.]
**Consequence:** [What happens if not addressed.]
**Recommendation:** [How to fix or what to investigate.]

## Medium

[Same finding structure as High.]

## Low

[Same finding structure as High.]
```

Rules:

- Group by severity. Never group by file or commit.
- Omit severity sections with zero findings.
- Within each section, order by estimated impact.

## Common Issues

### Dogmatic findings

Cause: Flagging DRY/YAGNI/SOLID violations without concrete impact.
Solution: Only report when there is a specific negative consequence
in the current context. Explain the impact, not the rule name.

### Cosmetic findings

Cause: Reporting style, formatting, or naming preferences.
Solution: Skip unless the choice causes genuine ambiguity
or misunderstanding.

### Over-triggering perspectives

Cause: Activating every conditional perspective on every review.
Solution: Only activate conditional perspectives
when the changeset contains relevant code.

## Examples

### Example: Review a branch

Input: "Review the unpublished changes on this branch."
Actions:

1. Obtain diff using project's source control tool.
2. Analyze diff, select perspectives (always-on + relevant conditional).
3. Review each perspective, record findings with severity.
4. Output report grouped by severity.
   Result: A structured report with Perspectives, Summary table,
   and findings grouped under High/Medium/Low headings.

### Example: Review a specific file

Input: "Review server.go"
Actions:

1. Read the file.
2. Select perspectives based on file content
   (e.g., Security, Performance, Error handling, Concurrency).
3. Review each perspective.
4. Output report.
   Result: Same report structure,
   but perspectives chosen based on file content
   rather than a diff.
