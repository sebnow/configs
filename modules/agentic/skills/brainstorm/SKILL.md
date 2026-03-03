---
name: brainstorm
description: "Explores ideas through structured questioning before any implementation. Use when brainstorming, requirements are unclear, or fleshing out ideas. Triggers: 'brainstorm', 'let us think about', 'flesh out this idea', 'explore options', unclear requirements, vague feature requests. Do NOT use for well-defined tasks with clear requirements."
---

# Brainstorming

Brainstorming is a multi-turn conversation
to understand a problem before solving it.

## Phase 1: Question

Respond with only questions. No solutions, no designs, no code,
no comparisons, no recommendations. Not even as context.

Ask about purpose, scope, constraints, and success criteria.
Group related questions (2-3 per message).
When you can enumerate reasonable options,
prefer multiple-choice questions.

Repeat until purpose, scope, constraints,
and success criteria are all explicitly confirmed by the user.
Not every brainstorm needs all four in depth —
calibrate to the problem.

If the user says "that's enough" or similar,
move to Phase 2 with what you have.

When neither you nor the user knows the answer,
record it as an open question for later research.
Do not guess. Do not fill gaps.

### Correct response

User: "I want to add caching to my API."

Response:
"A few questions before we dig in:
1. What is the API serving and what is the read/write ratio?
2. What is the motivation — latency, cost, DB load?"

### Incorrect response

User: "I want to add caching to my API."

Response:
"Here are the main caching strategies: cache-aside, write-through..."

This is incorrect because it proposes solutions
before understanding the problem.

## Phase 2: Confirm understanding

Summarize what you learned:

- Restate the problem in your own words
- List decisions made during the conversation
- List open questions and unknowns

Ask: "Is this accurate? Anything to add or correct?"
Do not proceed until confirmed.

## Phase 3: Write plan

Save to `.agents/plans/YYYY-MM-DD-<topic>-brainstorm.md`
where `<topic>` is a ticket reference (e.g., `GH-42`, `PROJ-123`)
or a short descriptive identifier (e.g., `auth-redesign`, `api-caching`).

The document does not need to be committed.

**Plan structure:**

```markdown
# Brainstorm: <title>

## Problem

[What problem this solves and why it matters]

## Decisions

[Key decisions made during the brainstorm, with reasoning]

## Scope

[What is in scope and what is explicitly excluded]

## Constraints

[Non-negotiables, limitations, requirements]

## Open Questions

[Unknowns requiring further research or discussion]

## Next Steps

[Concrete actions to move forward]
```

Scale sections to their relevance.
Omit sections that do not apply.

### Next steps

Always propose next steps, even if the user
only wanted to flesh out ideas. They can ignore them.

Next steps might include:

- Create an implementation plan
- Research open questions
- Prototype a specific aspect
- Write an ADR for a key decision
- Revisit after more thought

## Common Failures

- **Rushing to solutions** —
  Proposing implementations before understanding the problem.
  Stay in the problem space until the user confirms understanding.

- **Assuming domain knowledge** —
  Filling gaps with plausible details instead of asking.
  If you are uncertain, say so.

- **Over-questioning** —
  Asking about things already stated or obvious from context.
  Read first, then ask only about what remains unclear.
