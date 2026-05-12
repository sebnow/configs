---
id: 18-move-to-own-file
title: Single file accumulates handlers and becomes a merge-conflict bottleneck
fixture: fixtures/server-merge-conflicts
expect_trigger: true
target_lens: shotgun-surgery
category: pressure
rationale: |
  `server.go` holds nine handlers across four unrelated resources (reports,
  decisions, rulesets, queue items) plus the `Server` struct. The README
  records the operational signal that justifies splitting: four of the last
  ten merges resolved a conflict on this file. The friction is collaboration,
  not depth or duplication — the handlers are correct, the receiver is right,
  the package layout is deliberate. The catalog move is "Move to Own File on
  Collaboration Friction" — relocate handlers verbatim into per-resource
  files (`server_reports.go`, `server_decisions.go`, etc.) so concurrent
  edits stop colliding. Without the catalog the agent may flag the file as
  "too long" or propose a deeper redesign (sub-packages, new abstractions)
  on cosmetic grounds and miss the operational nature of the trigger.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: trigger-is-merge-conflicts-not-line-count
    text: |
      The agent identifies the trigger as operational — merge conflicts
      from concurrent edits, as recorded in the README's git-log note —
      not file length or cosmetic "too big" reasoning. It does not propose
      restructuring into sub-packages or new abstractions; the handlers
      remain on `(s *Server)` and the move is pure relocation.
  - id: named-move-move-to-own-file
    text: |
      The agent names the move it is proposing as
      "Move to Own File on Collaboration Friction" (or that exact catalog
      phrase), not just a generic "split this file" or "the file is too
      big" observation.
---

Please review this codebase and identify any refactoring candidates worth exploring.
