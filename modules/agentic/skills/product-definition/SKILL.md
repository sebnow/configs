---
name: product-definition
description: "Synthesizes a completed brainstorm transcript into a Product Requirements Document (PRD). Use when requirements are decided and a formal spec is needed. Triggers: 'create PRD', 'write PRD', 'product requirements', 'turn brainstorm into spec'. Do NOT use when requirements are still unclear — run the brainstorm skill first."
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

Use [assets/prd-template.md](assets/prd-template.md) as the structure.
It defines all 8 required sections in order with guidance and examples for each.

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

## Common Issues

### No brainstorm transcript available

Cause: User asked for a PRD without running a brainstorm session first.
Solution: Do not start an inline interview. Direct the user to the brainstorm skill, then return with the summary output.

### Module sketch lacks interface contracts

Cause: The brainstorm captured decisions but not the inputs, outputs, or error modes of each module.
Solution: Infer reasonable interfaces from the problem context. Flag each inferred contract as an open question in Open Questions so the team can validate before implementation begins.

### User wants to skip the approval step

Cause: User is eager to start implementation immediately after the PRD is written.
Solution: Remind the user that the PRD may contain incorrect assumptions. Skipping review risks wasted implementation work. Do not proceed without explicit approval.

### PRD sections contain implementation detail

Cause: Brainstorm captured file paths, code snippets, or database schemas that belong in technical design docs.
Solution: Move detail to Implementation Decisions as interface contracts only. Remove anything that specifies how a module works internally rather than what it accepts and returns.

## Examples

### Example: Brainstorm transcript → PRD

Input: User shares a brainstorm summary covering a problem restatement, three decisions, and two open questions.
Actions: (1) Read brainstorm output and synthesize decisions. (2) Sketch modules: for each module, name it, state its responsibility, and specify its inputs, outputs, and contracts. (3) Write the PRD in section order (Problem Statement → Solution → User Stories → Out of Scope → Implementation Decisions → Testing Decisions → Open Questions → Success Criteria). (4) Present the PRD and ask for approval.
Result: A complete PRD with module interfaces in Implementation Decisions, observable behaviors in Testing Decisions, and the two original open questions carried forward in Open Questions.

### Example: No brainstorm available

Input: User says "write a PRD for our new notification system" with no prior brainstorm.
Actions: Do not start an inline interview. Explain that a brainstorm transcript is required. Direct the user to run the brainstorm skill and return with its output.
Result: No PRD is started. User is unblocked toward the correct predecessor step.
