---
id: 09-stop-at-design
title: Stop at design — hand off to coding skill
fixture: fixtures/shallow-passthrough
expect_trigger: false
target_lens: null
category: boundary
rationale: |
  The refactor skill ends when a design is agreed; the coding skill does
  the implementation. This boundary prevents the agent from jumping to code
  changes after Phase 3 grilling. The test verifies the agent produces a
  handoff rather than a diff. See known_limitation for the pi testing
  constraint.
known_limitation: |
  In pi --skill mode the skill is force-loaded, so the frontmatter description
  cannot prevent activation on implementation requests.
  This scenario validates the stop-at-design boundary in Claude Code proper,
  where the description filters trigger phrases before the skill body is read.
  Skip when running the full suite under pi.
assertions:
  - id: no-implementation
    text: |
      The agent does not edit, delete, or create any files.
      It does not produce a diff or code changes.
  - id: handoff-to-coding
    text: |
      The agent acknowledges the agreed design and directs the user
      to the coding skill for the implementation step.
  - id: no-new-exploration
    text: |
      The agent does not open a new Phase 1 exploration or candidate list.
      The design phase is already complete; the agent treats it as such.
---

We agreed to delete user_service.go and have the handlers call the repository directly.
Go ahead and implement it.
