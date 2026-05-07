---
id: 19-aggressive-dead-code-removal
title: Legacy workflow remains after successor took over every caller
fixture: fixtures/dead-code-superseded
expect_trigger: true
target_lens: premature-abstraction
category: pressure
rationale: |
  `ApplyDecisionWorkflow` was superseded by `ProcessDecisionWorkflow` two
  months ago. Every gRPC handler and scheduled job calls the new workflow;
  the README and `server.go` confirm there are no callers of the legacy
  version, and Temporal history shows zero in-flight executions of it. The
  legacy file plus its exclusive activities (`ApplyDecision`,
  `EmitLegacyAudit`, `NotifyDownstreamLegacy`) are dead — kept "just in
  case" past the planned cutover. The catalog move is "Aggressive Dead-Code
  Removal After Successor Lands" — delete the legacy workflow and its
  exclusive activities in their own commit, now that the successor has
  fully landed and migrated. Without the catalog the agent may surface this
  as generic "dead code" or "unused symbol" cleanup without naming the
  introduce → migrate → delete sequence and without insisting the deletion
  ships in its own commit.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents a numbered list of refactoring candidates
      and asks which to explore further before proposing changes.
  - id: dead-not-just-unused
    text: |
      The agent identifies that `ApplyDecisionWorkflow` is dead because a
      named successor (`ProcessDecisionWorkflow`) has taken over every
      caller, not merely that the symbol is unused. It also recognizes the
      activities exclusive to the legacy workflow (`ApplyDecision`,
      `EmitLegacyAudit`, `NotifyDownstreamLegacy`) as part of the same
      dead surface.
  - id: named-move-aggressive-dead-code-removal
    text: |
      The agent names the move it is proposing as
      "Aggressive Dead-Code Removal After Successor Lands" (or that exact
      catalog phrase), not just a generic "remove dead code" or "delete
      unused workflow" observation. The proposal indicates the deletion
      ships in its own commit, separate from the introduce/migrate
      commits that landed the successor.
---

Please review this codebase and identify any refactoring candidates worth exploring.
