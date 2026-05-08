---
id: 04-hot-loop-linear-scan
title: Linear scan inside a hot loop
fixture: fixtures/hot-loop-linear-scan
expect_trigger: true
target_lens: non-pessimization
category: pressure
rationale: |
  A linear scan called per-row in a hot path is structural overhead that
  belongs to the non-pessimization lens, not micro-optimisation. The failure
  mode is the agent ranking it below style or naming issues, or missing it
  entirely when other noise is present. This scenario verifies the lens
  elevates structural performance friction to the top of the candidate list.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: hot-loop-identified
    text: |
      The agent identifies that IsAdmin (or equivalent linear scan)
      is called inside a per-row or per-item loop.
  - id: map-or-set-suggested
    text: |
      The agent suggests replacing the linear scan with a map or set lookup
      to achieve O(1) role checking.
---

Please review this codebase and identify any refactoring candidates worth exploring.
