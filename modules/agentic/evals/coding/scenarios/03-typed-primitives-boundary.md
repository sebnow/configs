---
id: 03-typed-primitives-boundary
title: Public boundary uses named identifier types, not raw strings
fixture: fixtures/typed-primitive-boundary
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `ReportContent` is the public entry point of a moderation service. Two
  of its parameters — the reporting user and the reported content — are
  semantically distinct identifiers but both arrive as `string`. The
  prompt asks only for input validation: parse the UUIDs and the enum.
  The Go idiom is to give each identifier a named type with a `Parse*`
  (or `New*`) constructor that returns the named type and an error. That
  shifts the validation from a one-off step inside `ReportContent` to a
  property of the type itself: any value of `UserID` is already a valid
  user identifier, so the boundary function does not need to re-check.
  Without the typed-primitives-at-boundaries rule encoded in the coding
  skill, agents tend to add the parsing inline (`uuid.Parse(reporterUserID)`
  inside `ReportContent`) and leave the parameters as raw `string`. The
  validation runs once but the type at the boundary stays `string`, so
  every future caller has to remember which parameter is which.
assertions:
  - id: named-types-introduced
    text: |
      The agent declares at least two distinct named identifier types
      (e.g. `type UserID string` and `type ContentID string`, or
      semantically-similar names like `UserReference`/`ContentReference`,
      `ReporterID`/`ReportedContentID`). The two named types are
      separately declared (not aliases of each other) so the compiler
      treats them as non-interchangeable. Wrapping both identifiers
      inside one struct does not satisfy this assertion — the underlying
      parameter types must change to distinct named types.
  - id: parse-constructors
    text: |
      The agent introduces a parse-style constructor for each named
      identifier type (e.g. `ParseUserID`, `NewUserID`, `ParseContentID`,
      `NewContentID`) that takes a raw `string`, performs the UUID
      validation, and returns either the named type and `error` or a
      domain-specific error if the input is invalid.
  - id: signature-uses-named-types
    text: |
      The final signature of `ReportContent` (or its successor entry
      point if the agent renames it) accepts the two named identifier
      types directly as parameters, not raw `string`. Callers reaching
      `ReportContent` must hold values of the named types — not strings
      that happen to validate at the function boundary.
---

Implement input validation for `ReportContent`: each identifier must be a
non-empty UUID, and the reason code must be one of `spam`, `harassment`,
`violence`. Return an error that indicates which parameter was invalid. Do
not implement the persistence step.
