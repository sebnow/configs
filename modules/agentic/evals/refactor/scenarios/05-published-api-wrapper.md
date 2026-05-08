---
id: 05-published-api-wrapper
title: Published API - external consumer boundary
fixture: fixtures/published-api-wrapper
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  Proposing breaking changes to a published interface without acknowledging
  the cost is a high-severity mistake when external integrators depend on
  the contract. The fixture comment signals that ProcessOrder is part of a
  public SDK. This scenario verifies the agent reads that signal and
  constrains suggestions to internal restructuring, not signature changes.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and ends the message with a question asking which candidate
      (or which direction within a candidate) to explore. A one-sentence
      solution sketch within the candidate description is acceptable and
      does not violate the pause requirement.
  - id: api-boundary-recognized
    text: |
      The agent notes that ProcessOrder is the package's public API
      and that it is shipped to external consumers.
  - id: external-constraint-respected
    text: |
      The agent constrains its suggestions to internal restructuring
      and does not propose changes to the exported ProcessOrder signature
      without acknowledging the breaking-change risk.
---

Please review this codebase and identify any refactoring candidates worth exploring.
