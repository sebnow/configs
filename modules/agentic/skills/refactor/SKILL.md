---
name: refactor
description: "Drives the design phase of a refactor. Walks the codebase through friction lenses and surfaces architectural refactoring candidates. Use when the user asks to 'refactor', 'improve architecture', or 'find refactoring opportunities'. Do NOT use for code formatting, naming-only changes, or implementing already-decided refactors — use the coding skill for implementation."
---

# Refactor

This is the design phase of a refactor.
The coding skill handles implementation.

## Vocabulary

Use these terms precisely throughout the process:
module, interface, implementation, API boundary, depth, leverage, locality, adapter, port.
Say "API boundary", never "seam".
For full definitions and terms to avoid, read [references/vocabulary.md](references/vocabulary.md).

## Process

### Phase 1: Explore

Walk the codebase asking each friction question below.
Surface candidates only where a lens fires on real architectural friction —
not on style, naming, magic numbers, or hypothetical scenarios.
If no lens fires, say "no architectural friction surfaced" and stop.
Do not stretch a lens to fit idiomatic code.

- Shallow modules — where is the interface nearly as complex as the implementation?
- Shotgun surgery — where does a single conceptual change require edits across many modules? Wrapping exploded parameters in a config struct is NOT a fix — adding a sixth field must still be edited everywhere. The fix is to name the variants so a new one requires only one edit.
- Hoisting — where is the same decision evaluated repeatedly through a tree, loop, or call chain?
- Compression — where does the same logic appear two or more times without being shared?
- Premature abstraction — where is there an interface with one implementation, a helper called once, or a generic with one type?
- Non-pessimization — where are we paying gratuitous overhead the structure forces (loop-invariant work inside the loop, per-request allocation of a shared resource)?
- Learning ladder (gated) — apply ONLY when consumers are not fully visible: a published library, package, or service with external callers. Is the convenience wrapper the only path, or can callers reach the lower-level pieces? If every caller is inside this repo and visible to grep, do NOT apply this lens — use compression and premature-abstraction instead.

For detection patterns, smell descriptions, and before/after examples for each lens,
read [references/principles.md](references/principles.md).

### Phase 2: Present candidates

For named moves to use in the solution sketch,
read [references/transformations.md](references/transformations.md).

Present a numbered list. For each candidate:
- files involved
- the problem in domain terms
- a one-sentence solution sketch that names the move from the catalog when one fits
- locality/leverage benefit

Do not propose interface signatures yet.
Ask which candidate to explore.

### Phase 3: Grilling loop

Grill the chosen candidate into a concrete design.

Read [references/dependency-strategy.md](references/dependency-strategy.md)
to determine the dependency category and adapter requirements.

Two tests every candidate must pass before proposing a design:

- Deletion test — would deleting this module concentrate complexity across N callers, or just move it? If "just move it", the module is shallow.
- Compression test — does the candidate collapse repeated logic, or just relocate it?
- Two-adapters test — one adapter is a hypothetical port; two is a real one. Do not introduce a port for a single implementation.

If the candidate is high-stakes (long-lived interface, many callers, non-obvious design choice),
read [references/design-alternatives.md](references/design-alternatives.md)
and spawn three or more sub-agents in parallel, each with a different design constraint.

End Phase 3 with an agreed design. Then hand off to the coding skill for implementation.

## Side effects

After each candidate is resolved:

- New domain terms introduced: offer to add to the project glossary.
- Rejected candidates with a load-bearing reason: offer to record as an ADR using the `adr-writing` skill.
