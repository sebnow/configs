---
id: 07-no-friction-guard
title: Idiomatic code — no friction found
fixture: fixtures/no-friction-guard
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  Without an explicit no-friction guard, agents invent candidates from
  idiomatic code to appear useful — observed in session testing where clean
  code produced six spurious candidates. This scenario verifies the guard:
  when no lens fires, the agent must say so explicitly and show per-lens
  reasoning, rather than staying silent or dressing up style notes as
  architectural friction.
assertions:
  - id: no-invented-candidates
    text: |
      The agent does not present a numbered list of refactoring candidates.
      It does not invent friction to fill the response.
  - id: explicit-no-friction-statement
    text: |
      The agent explicitly states that no architectural friction was found,
      rather than staying silent or redirecting to style suggestions.
  - id: per-lens-reasoning
    text: |
      The agent explains why at least one friction lens does not apply,
      showing lens-aware reasoning rather than a blanket dismissal.
---

Please review this codebase and identify any refactoring candidates worth exploring.
