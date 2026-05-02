---
name: product-definition
description: "Use after brainstorming to consolidate decisions into a Product Requirements Document. Synthesizes a brainstorm transcript into a PRD. Triggers: 'create PRD', 'write PRD', 'product requirements', 'turn brainstorm into spec'. Do NOT use when requirements are still unclear — run the brainstorm skill first."
---

# Product Definition

Consolidate a completed brainstorm into a Product Requirements Document (PRD).

## Predecessor: Brainstorm

This skill assumes a brainstorm transcript already exists —
the structured output of the `brainstorm` skill:
problem restatement, decisions made, open questions.
That transcript is the input to this skill.

If no brainstorm transcript exists, do not start an inline interview.
Direct the user to brainstorm first:

> "Before writing a PRD I need a brainstorm transcript:
> the problem restatement, decisions, and open questions.
> Run the brainstorm skill first, then return with that summary."

## Workflow

You must follow this sequence:

1. **Read brainstorm output** — synthesize what is already decided. Do not re-interview.
2. **Sketch modules** — identify deep modules and their public interfaces.
3. **Write PRD** — fill the required structure using the brainstorm and module sketch.
4. **Get approval** — never begin implementation before user approval.

Synthesize from the brainstorm transcript. Do not ask new clarification questions.
If a critical decision is missing, flag it as an open question and continue.
The user can return to brainstorm if needed.

## Step 2: Sketch Modules

Before writing the PRD, identify the deep modules
this work introduces or modifies.
A deep module has a narrow public interface and substantial internal complexity.

For each module, capture:

- **Name** — what the module is called
- **Responsibility** — one sentence
- **Inputs** — what callers pass in (types or shapes)
- **Outputs** — what the module returns or produces
- **Contracts** — invariants, error modes, side effects

Specify interfaces, not internals.
Downstream agents own the implementation.

Do not include file paths, line numbers, or code snippets.
Those belong in the issues produced from this PRD.

The module sketch feeds the Implementation Decisions section
of the PRD (see below).

## PRD Structure

Required sections in this order:

### 1. Problem Statement

State the problem and who has it.
Plain language. No solutions yet.

### 2. Solution

Describe the chosen approach in one or two paragraphs.
Reference decisions captured in the brainstorm.
Avoid implementation detail — interfaces and contracts go in section 5.

### 3. User Stories

Describe how users will interact with the feature.
Format: "As a \[user type], I want to \[action] so that \[benefit]"

Include realistic usage scenarios.

### 4. Out of Scope

Explicitly list what this feature will NOT include.

This section must contain at least one non-trivial exclusion —
something a reasonable reader might otherwise assume is in scope.
Empty or filler exclusions defeat the purpose.

This section sits above implementation details deliberately:
scope decisions constrain everything that follows.

Example:

```
Out of scope for this release:
- AI-powered priority suggestions (postpone to next quarter)
- Bulk priority assignment via CSV import
- Priority history and audit trail
- Mobile-specific priority gestures
```

### 5. Implementation Decisions

Captures the module sketch from Step 2 and any architectural decisions
made during brainstorm.

Required content:

- Modules to build or modify, with their public interfaces
  (inputs, outputs, contracts) — from the module sketch
- Architectural decisions and the reasoning behind them
- Cross-cutting contracts (auth, persistence, error handling) if relevant

No file paths, line numbers, or code snippets.
Downstream issues will pin those down.

Organize this section so issues can be sliced vertically —
each module entry should map to one or more end-to-end issues
that exercise the module from a user-visible behavior.

### 6. Testing Decisions

Captures which modules get automated tests
and what behavior to verify.

Required content:

- Which modules from Implementation Decisions get tests
- Prior-art reference: tests in the codebase that demonstrate
  the testing style for this kind of module
- Behavior-vs-implementation guidance:
  what to assert (observable behavior)
  vs. what not to assert (internal structure)

### 7. Open Questions

List anything still unclear after brainstorm.
These must be resolved before implementation begins.

### 8. Success Criteria

How will we know this feature works?
Include measurable indicators.

Example:

```
- 60% of users assign priorities within first week
- Task selection time decreases by 30%
- Zero priority-related bugs in first month
```

## Language Requirements

Target junior developers as primary readers.

Required:

- Simple, declarative sentences
- Plain English (avoid: "utilize", "leverage", "facilitate")
- Concrete examples over abstract concepts
- Explicit over implicit

Forbidden:

- Technical jargon without explanation
- Code snippets in requirements
- Vague terms ("user-friendly", "intuitive", "seamless")
- Passive voice ("shall be provided", "is enabled")

## Common Mistakes

**Over-specification**
Do not include:

- Competitive analysis
- Technical architecture
- Database schemas
- API specifications
- Implementation timelines

These belong in separate technical design documents.

**Under-specification**
Every requirement must be:

- Testable (can verify it works)
- Unambiguous (one interpretation only)
- Necessary (supports stated objectives)

## Validation Checklist

Before saving PRD, verify:

- [ ] A brainstorm transcript was used as input
- [ ] Module sketch exists and informs Implementation Decisions
- [ ] All required sections present in the specified order
- [ ] Out of Scope contains at least one non-trivial exclusion
- [ ] Implementation Decisions specifies interfaces, not file paths or code
- [ ] Testing Decisions identifies which modules get tests
- [ ] No file paths, line numbers, or code snippets anywhere in the PRD
- [ ] User asked for approval before issue breakdown begins

## After PRD Creation

State: "PRD complete. Please review and approve before issue breakdown."

Wait for explicit approval.
Do not proceed with any coding, architecture, or design work until user confirms.

### Lifecycle

The PRD is an intermediate artifact.
Once issues are produced from it:

- Move the PRD out of the active documentation path
  (for example, into an `archive/` or `done/` directory)
- Do not retain it where future agents will read it as authoritative

Stale specifications mislead more than absent ones.
Issues become the authoritative description of the work;
the PRD has served its purpose.

If the user asks "what now?" after PRD approval,
the next step is issue breakdown, then archival of this PRD.

## Downstream Consumer

The PRD feeds an issue-breakdown step (separate skill, not this one).
That step produces vertically-sliced issues
that an executing agent can act on without re-exploring the codebase.

To support this:

- Implementation Decisions must let an agent identify
  the affected module(s) per issue
- Testing Decisions must let an agent know which behavior to verify
- Out of Scope must let an agent reject creep without re-asking

If a fresh agent cannot draft 3 vertical-slice issues from the PRD alone,
the PRD is incomplete — return to Step 2 and tighten module interfaces.
