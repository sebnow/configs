---
id: 20-promote-primitive-to-named-type
title: User and content IDs both carried as raw strings
fixture: fixtures/promote-primitive-named-type
expect_trigger: true
target_lens: shotgun-surgery
category: pressure
rationale: |
  `BlockUser`, `AssignReviewer`, and the surrounding handlers carry user IDs,
  content IDs, and reporter IDs as raw `string` parameters. Three semantic
  kinds collapse into one type, so callers can transpose arguments at the
  call site and the program compiles cleanly while routing the action against
  the wrong subject. Validation rules ("non-empty", caller-supplied prefixes)
  are re-derived inside each function instead of living on a parse
  constructor. The catalog move is "Promote Primitive to Named Type" — give
  each kind its own type (`UserReference`, `ContentReference`,
  `ReporterReference`) with parse constructors so the compiler rejects
  transposition and validation lives in one place. Without the catalog the
  agent may flag this as "weak typing" or "primitive obsession" without
  naming the move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents a numbered list of refactoring candidates
      and asks which to explore further before proposing changes.
  - id: distinct-kinds-share-type
    text: |
      The agent observes that user IDs, content IDs, and reporter IDs are
      three distinct semantic kinds carried as the same primitive `string`
      type, and that this allows transposition at call sites such as
      `BlockUser(ctx, userID, contentID, reporterID)` to compile silently.
      It distinguishes these from `correlationID`, which is genuinely an
      opaque per-request value and should remain a raw string.
  - id: named-move-promote-primitive-to-named-type
    text: |
      The agent names the move it is proposing using the verbatim catalog
      phrase "Promote Primitive to Named Type". A paraphrase such as
      "promote raw ID strings to named types", "introduce a value object",
      "primitive obsession", or "use named ID types" does NOT satisfy this
      assertion — the agent must reproduce the exact catalog phrase.
      The proposal must also indicate that the named types come with
      parse constructors so validation lives in one place.
---

Please review this codebase and identify any refactoring candidates worth exploring.
