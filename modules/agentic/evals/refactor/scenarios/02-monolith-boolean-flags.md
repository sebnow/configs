---
id: 02-monolith-boolean-flags
title: Monolith with boolean variant flags
fixture: fixtures/monolith-boolean-flags
expect_trigger: true
target_lens: hoisting
category: pressure
rationale: |
  Boolean flags threaded through callers mean each call site already knows
  which variant it wants; the branching belongs there, not in a single
  polymorphic function. The key failure mode is the agent bundling the flags
  into a struct (bundling-as-fix anti-pattern) rather than pushing the
  decision to the call site. This scenario guards against that substitution.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents a numbered list of refactoring candidates
      and asks which to explore further before proposing changes.
  - id: boolean-flag-function-identified
    text: |
      The agent identifies the multi-boolean-parameter render function
      as a refactoring candidate.
  - id: hoisting-lens-applied
    text: |
      The agent notes that callers each use a different combination of flags,
      suggesting the branching logic belongs at the call site
      or in distinct functions rather than in a single polymorphic function.
---

Please review this codebase and identify any refactoring candidates worth exploring.
