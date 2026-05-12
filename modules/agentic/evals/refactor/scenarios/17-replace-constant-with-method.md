---
id: 17-replace-constant-with-method
title: Constant identifies a type but is declared outside it
fixture: fixtures/constant-names-type
expect_trigger: true
target_lens: shotgun-surgery
category: pressure
rationale: |
  Each workflow update message has a free-floating `const ...MessageName` whose
  only job is to name the adjacent struct. The constant and the struct must
  always be renamed together but the type system enforces nothing — the link
  is a naming convention. The catalog move is "Replace Constant with Method
  on the Owning Type" — give the struct an `UpdateName()` method that returns
  the literal so the name and the struct cannot diverge. Without the catalog
  the agent may flag the constant pair as "naming convention" or "duplication"
  without naming the move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: constant-identifies-the-type
    text: |
      The agent observes that each `...MessageName` constant identifies its
      adjacent struct (e.g. `AppealOutcomeUpdateMessageName` exists only to
      name `AppealOutcomeUpdateMessage`) and that the two must be renamed
      together but nothing in the type system enforces this. It distinguishes
      these from `DefaultTaskQueueName`, which is genuinely process-wide
      configuration not owned by a single type and should remain a constant.
  - id: named-move-replace-constant-with-method
    text: |
      The agent names the move it is proposing as
      "Replace Constant with Method on the Owning Type" (or that exact
      catalog phrase), not just a generic "naming convention" or
      "couple the constant to the struct" observation.
---

Please review this codebase and identify any refactoring candidates worth exploring.
