---
id: 08-learning-ladder-gated
title: Learning-ladder lens gated by caller visibility
fixture: fixtures/learning-ladder-gated
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The learning-ladder lens applies only when external consumers cannot reach
  low-level building blocks. When all callers are visible in the repo, the
  lens must not fire — proposing exported internals would introduce surface
  area with no benefit. Scenario 05 tests the positive path; this tests the
  gate. It is a regression risk: the gate was added after the lens was
  observed firing on internal code during session testing.
assertions:
  - id: learning-ladder-not-applied
    text: |
      The agent does not flag a learning-ladder violation.
      It does not propose exporting unexported helpers
      as an escape hatch for advanced callers.
  - id: gate-acknowledged
    text: |
      The agent notes that all callers are visible within this repository
      and skips the learning-ladder lens on that basis.
  - id: alternative-lenses-considered
    text: |
      The agent applies compression or premature-abstraction lenses instead,
      or concludes explicitly that no lens fires for this codebase.
---

Please review this codebase and identify any refactoring candidates worth exploring.
