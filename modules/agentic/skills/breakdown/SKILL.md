---
name: breakdown
description: "Decomposes a PRD, brainstorm transcript, or raw requirements into vertically-sliced implementation issues with context, acceptance criteria, and dependency ordering. Use when starting implementation planning from any requirement input. Triggers: 'break into issues', 'create issues', 'decompose PRD', 'write issues', 'issue breakdown', 'breakdown'. Do NOT use when requirements are still unclear — run brainstorm skill first."
---

# Issue Breakdown

Turn requirements into vertically-sliced implementation issues
that an agent can act on without re-exploring the codebase.

## Workflow

Follow these steps in order. Do not skip steps.

### Step 1: Extract behavioral goals

Read the input — PRD, brainstorm transcript, or raw chat.

For each user-visible behavior the input describes, extract:

- The observable outcome
- Who observes it
- Any constraints (auth, data shape, error cases)

Flag anything that is missing or ambiguous rather than filling the gap.
Preserve all uncertainty markers from the input.

### Step 2: Decompose into vertical slices

Each issue must deliver one end-to-end user-visible behavior.

A vertical slice touches every layer needed to produce the behavior —
UI, API, persistence — in a single issue.
A horizontal slice touches only one layer (e.g., "implement DB schema") — forbidden.

Test: if you implemented only this issue, would a user observe something working? If not, re-slice.

Do not create issues for:

- Shared abstractions or interfaces as prerequisites — that is premature abstraction
- Test plans — testing belongs to the testing skill
- Effort estimates

### Step 3: Explore relevant codebase boundaries

Before formatting issues, explore the codebase to discover
existing API boundaries, types, and signatures relevant to each slice.

Capture:
- File paths where relevant code lives (labeled as orientation hints, not authoritative)
- Existing types, function signatures, or contracts the implementing agent should know about
- Proposed new boundaries as advisory guidance, not mandated abstractions

### Step 4: Order by behavioral dependency

Order issues so each one can be implemented without mocking a dependency,
when possible.

For each dependency:

- Name which issue it depends on
- State why the dependency exists
- State under what circumstances the issue could proceed without it

When ordering cannot avoid a mock,
create an explicit integration follow-up issue to replace the mock.
Never leave deferred integration implicit.

### Step 5: Format and present draft

Format each issue using the template in [assets/issue-template.md](assets/issue-template.md).

Present the full draft in the conversation.
Do not publish to any tracker — that is the user's responsibility.

Wait for user feedback before finalizing.

## Acceptance Criteria Rules

Acceptance criteria must specify the exact observable behavior that proves the slice is done:

- A specific HTTP response (status, shape)
- A visible UI change
- A file on disk
- A measurable metric

Forbidden in acceptance criteria:

- "Tests pass"
- "Code is reviewed"
- "Type checks pass"
- "Covered by a test"
- General engineering standards

These are assumed. Do not list them.

## Anti-Patterns

Never:

- Create a "DB layer" issue, "API layer" issue, or "tests" issue — these are horizontal
- Create a shared interface as a prerequisite issue — this is premature abstraction
- Invent decisions that are missing from the input — flag them
- Leave a mock in place without a follow-up integration issue
- Write acceptance criteria that only describe code quality, not observable behavior

## Common Issues

### Input is ambiguous or incomplete

Cause: the PRD or chat transcript did not capture all decisions.
Solution: flag each missing decision as an open question in the relevant issue's Context section. Do not invent answers. Do not ask the user — record the gap and continue.

### Two issues have a circular dependency

Cause: the decomposition is too coarse or the slice boundary is wrong.
Solution: re-examine the slices. Usually one can be made thinner so it does not depend on the other, or one is actually a prerequisite that can be extracted as a first slice with no dependencies.

### Mock is unavoidable

Cause: an external service or a later issue's output is needed before that issue exists.
Solution: create an explicit integration follow-up issue titled "Replace [mock] with real [service]" and add it to the draft. Note it in the blocking issue's Blocked by section.

## Examples

### Example: PRD → vertical slices

Input: PRD describing a /health endpoint (no auth) and a /users endpoint (Bearer token, returns id+email from DB).
Actions: (1) Extract two behavioral goals: health probe, authenticated user listing. (2) Decompose: Issue 1 = /health returns 200 {status:ok}; Issue 2 = /users returns 401 without token; Issue 3 = /users returns user list with valid token. (3) Explore codebase for Express router patterns, auth middleware location, DB query conventions. (4) Order: Issue 1 has no deps; Issue 2 depends on auth middleware (why: endpoint behavior is undefined without it; bypass: stub middleware); Issue 3 depends on Issues 1 and 2 (why: both must exist to test the full path). (5) Format with template and present draft.
Result: Three issues, each vertically sliced, with file hints and specific AC. No "implement DB schema" issue.

### Example: Missing information in input

Input: user says "add filters to the dashboard but I am not sure if server-side or client-side yet."
Actions: Extract the behavioral goal (user can filter dashboard rows). Flag "filter location (server-side vs client-side) is undecided — this affects the issue split" as an open question in the issue's Context. Produce a single placeholder issue covering the end-to-end filter behavior, noting the open question.
Result: One issue with an explicit open question. No invented decision. No two separate issues for each approach.

### Example: Mock creates integration follow-up

Input: checkout flow that calls Stripe, which is unavailable in dev.
Actions: Decompose into Issue 1 = checkout endpoint calling Stripe client; Issue 2 = end-to-end Stripe integration. Mark Issue 1 as using a mock Stripe client. Create Issue 2 titled "Replace Stripe mock with real Stripe client" blocked by Issue 1. Note Issue 2 in Issue 1's Blocked by.
Result: Issue 1 can be implemented without live Stripe. Issue 2 explicitly closes the deferred integration gap.
