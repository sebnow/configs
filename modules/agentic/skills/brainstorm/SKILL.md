---
name: brainstorm
description: "Explores ideas through structured questioning to reach shared understanding. Use when brainstorming, requirements are unclear, or fleshing out any idea. Triggers: 'brainstorm', 'let us think about', 'flesh out this idea', 'explore options', unclear requirements, vague requests. Do NOT use for well-defined tasks with clear requirements."
---

# Brainstorming

Brainstorming is a structured interview
to reach shared understanding before taking any action.

## Phase 1: Question

Respond with only questions. No solutions, no designs, no code,
no comparisons, no recommendations. Not even as context.

Walk down the decision tree, resolving dependencies between decisions one by one.
For each batch of questions, provide your recommended answer.

If a question can be answered by exploring the codebase or other available sources,
do that instead of asking.

Group related questions (2-3 per message).

Continue until you have a clear picture of
what the user is trying to achieve,
why they care,
and what constraints apply.
Not every brainstorm needs exhaustive coverage —
calibrate to the problem.

If the user says "that's enough" or similar,
move to Phase 2 with what you have.

When neither you nor the user knows the answer,
record it as an open question.
Do not guess. Do not fill gaps.

### Correct response

User: "I want to add caching to my API."

Response:
"A few questions before we dig in:
1. What is the API serving and what is the read/write ratio? (My guess: read-heavy)
2. What is the motivation — latency, cost, DB load? (My guess: DB load)"

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
