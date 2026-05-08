---
id: 01-shallow-passthrough
title: Shallow pass-through module
fixture: fixtures/shallow-passthrough
expect_trigger: true
target_lens: shallow-modules
category: pressure
rationale: |
  The deletion test is the primary diagnostic for shallow modules:
  imagine deleting user_service.go — does complexity vanish, or reappear
  across the callers? A pass-through wrapper that adds no business logic
  fails the test. Without this scenario the skill might explore the codebase
  but never name the deletion test or conclude a module is earning its keep.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and ends the message with a question asking which candidate
      (or which direction within a candidate) to explore. A one-sentence
      solution sketch within the candidate description is acceptable and
      does not violate the pause requirement.
  - id: deletion-test-applied
    text: |
      The agent applies the deletion test to the wrapper module
      and concludes that user_service is shallow or a pass-through.
  - id: vocabulary-correct
    text: |
      Uses "API boundary" rather than "seam".
      Uses depth, leverage, or locality with their structural meanings.
---

Please review this codebase and identify any refactoring candidates worth exploring.
