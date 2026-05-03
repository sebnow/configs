---
id: 03-single-impl-interface
title: Single-implementation interface
fixture: fixtures/single-impl-interface
expect_trigger: true
target_lens: premature-abstraction
category: pressure
rationale: |
  One implementation means the interface is hypothetical: the abstraction
  cost is paid before any benefit exists. The two-adapters test formalises
  this — one adapter is a hypothetical port; two adapters make it real.
  The failure mode is the agent recommending adding more adapters to justify
  the interface rather than questioning whether the interface is warranted.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents a numbered list of refactoring candidates
      and asks which to explore further before proposing changes.
  - id: single-impl-identified
    text: |
      The agent identifies the UserRepo interface as having only one
      production implementation with no test double or second adapter.
  - id: second-impl-test-applied
    text: |
      The agent asks whether a second implementation is planned or likely,
      or explicitly notes that the interface abstraction may be premature.
---

Please review this codebase and identify any refactoring candidates worth exploring.
