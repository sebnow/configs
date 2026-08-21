---
name: design-architecture
description: "Designs the boundary structure of a change before code is written: where boundaries sit, which way dependencies point, and what crosses each boundary. Use when a change is decided and needs its shape designed, or when refactor hands off a boundary-level candidate. Triggers: 'design the architecture', 'where should the boundary go', 'design-architecture', a refactor boundary candidate. Do NOT use to find friction in existing code (use refactor), to pin exact signatures and call flow (use blueprint), or to implement (use coding)."
disable-model-invocation: true
---

# Design Architecture

Design the shape of a change before the code exists: where the boundaries sit,
which way dependencies point, and what crosses each boundary — in the terms of
the change itself. Reason from the change in front of you.

It does not implement, and it does not pin exact signatures or call flow —
those come after (blueprint, then coding).

## What this is not

- Not `refactor`. Refactor finds friction in code that already exists and names a
  fix. This skill shapes code that does not exist yet, so no friction has
  accumulated to find.
- Not `blueprint`. Blueprint pins each module's exact signature and the call flow.
  This skill decides the boundary structure blueprint then records.
- Not `coding`. No implementation, no line-level work.

## Vocabulary

Plain, style-agnostic terms: module, interface, implementation, boundary,
dependency direction, depth, leverage, locality.
See [references/vocabulary.md](references/vocabulary.md).

## Input

One of:

- A **decided change** — a brainstorm output, ticket, or prior conversation that
  settled what to build and why. This skill decides the shape, not the intent.
- A **boundary candidate from `refactor`** — refactor found friction and named a
  boundary-level fix. This skill designs that boundary.

If the intent is undecided, stop and route to `brainstorm`. Do not shape a change
nobody has agreed to make.

## Workflow

1. **Map the modules and dependency direction.** List the modules the change
   introduces or touches. For each dependency between them, state which side
   knows about the other. Apply the Direction lens; fix the map before continuing
   if a dependency points the wrong way.
2. **Draw each boundary.** For every boundary, decide and record: which way it
   faces, what varies below it (what it hides), what crosses it (in the
   consumer's terms), and what leaks. Run each boundary through the Lenses.
3. **Explore alternatives for high-stakes boundaries** — a long-lived interface,
   many callers, or a non-obvious choice. See
   [references/design-alternatives.md](references/design-alternatives.md).
4. **Write the sketch, get approval, hand off.** Produce the architecture sketch
   (see Output), then hand to `blueprint` — or, for a simple boundary whose
   signatures are obvious, straight to `coding`.

## Lenses

Run every boundary through all five. They shape what to build; they are not
smells to detect in existing code.

### Direction of dependency

Point each dependency deliberately, one way. The module that owns a decision
keeps it; the module it depends on reports outcomes and does not reach back to
make the caller's choices — when to retry, when to fall back, what to do next.
Prefer pointing dependencies toward the parts that change least.
Watch for: a depended-upon module that decides the caller's control flow, or a
caller reaching past an interface into the concrete thing behind it.

### Hide what varies

Draw a boundary around the decision most likely to change, so the interface holds
while the code behind it moves. Hide what is known to vary — an external format,
a storage shape, a lookup, a third-party tool. Do not draw a boundary around
something that will not vary: an interface with one implementation and no second
in sight is indirection without payoff. A second real implementation justifies an
interface; the mere possibility of one does not. For judging how much a
dependency varies, see
[references/dependency-strategy.md](references/dependency-strategy.md).

### Cross the boundary in the consumer's terms

What passes through a boundary is expressed in the terms of the side that
receives it, not the internals of the side that produces it. The producing side
resolves, transforms, and formats below the boundary — which is how the varying
detail stays hidden. Concretely:

- Do not pass a whole upstream object across because it happens to hold the
  needed fields. Pass only what the consumer needs, named in the consumer's terms.
- Do not let a transport or external format cross. Format below the boundary,
  where the format is free to change.
- Resolve derived values (an id to a name) once, on the producing side. The
  consumer does not run a second pass to resolve what the producer could have.

### Build the primitive first

Build the smaller, direct capability first. A richer, convenience layer can wrap
over it later — but a convenience layer cannot be taken apart back into the
pieces it swallowed. So expose the direct capability, and do not force a choice
on the caller (how to allocate, how to format, how to batch) that a lower-level
surface would leave open. Test: never offer only a high-level operation that a
few lower-level ones could not reconstruct.

### Expect leakage

No boundary hides everything. Timing, failure behaviour, and edge cases seep
through even a clean interface. Name what leaks across each boundary, and confirm
the boundary still pays for itself. The freedom to change the code behind an
interface is real, but it stops at the interface — not at the behaviour callers
can observe.

For the reasoning and sources behind each lens, see
[references/lenses.md](references/lenses.md).

## Output: the architecture sketch

A short document that feeds `blueprint`:

- **Modules** — each with a one-sentence responsibility.
- **Boundaries** — for each: which way it faces, what it hides, what crosses it in
  the consumer's terms, whether a separate interface is justified, and what leaks.
- **Decisions** — each boundary choice and why, naming the lens that drove it.

Leave out exact signatures, call flow, implementation, schemas, configuration,
file paths, and line numbers. Those belong to blueprint and coding.

## Check before handing off

- Every dependency points deliberately; none of the wrong way.
- Every boundary hides something that varies; none is speculative.
- Nothing crosses a boundary in the producer's internal terms — no whole upstream
  objects, no transport formats leaking up.
- Resolution and formatting sit on the producing side.
- No interface exists for a single implementation.
- Leakage is named for each boundary.
- No signatures, call flow, or implementation detail in the sketch.

## After the sketch

State: "Architecture sketch complete. Please review and approve before blueprint."
Wait for explicit approval before any blueprint, coding, or implementation work.

## Downstream

- `blueprint` — pins each module's exact signature and the call flow.
- `coding` — for a simple boundary with obvious signatures, implement directly.
