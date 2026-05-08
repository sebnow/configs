---
id: 15-replace-wrapper-library
title: Custom wrapper duplicates http.Client
fixture: fixtures/library-wrapper-doer
expect_trigger: true
target_lens: premature-abstraction
category: pressure
rationale: |
  The Doer interface and the Client wrapper add nothing the standard library
  does not already provide: every method forwards directly to httpClient.Do,
  no production code substitutes a second Doer, and no test uses a fake.
  The catalog move is "Replace Custom Wrapper with Library" — delete the
  wrapper and call *http.Client directly. Without the catalog the agent may
  surface this as generic "premature abstraction" or "shallow module" without
  naming the move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: wrapper-duplicates-library
    text: |
      The agent observes that the Client wrapper and the Doer interface
      duplicate behavior already provided by *http.Client / the net/http
      package, and that the wrapper adds no business logic of its own.
  - id: named-move-replace-wrapper
    text: |
      The agent names the move it is proposing as
      "Replace Custom Wrapper with Library" (or that exact catalog phrase),
      not just a generic "this looks shallow" or "premature abstraction"
      observation.
---

Please review this codebase and identify any refactoring candidates worth exploring.
