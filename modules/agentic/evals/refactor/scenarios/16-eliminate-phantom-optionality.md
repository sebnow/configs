---
id: 16-eliminate-phantom-optionality
title: LEFT JOIN forces nullable scan the domain forbids
fixture: fixtures/phantom-optionality-leftjoin
expect_trigger: true
target_lens: shotgun-surgery
category: pressure
rationale: |
  The repository scans every rule column into a `*T` and then unwraps the
  pointers behind a `if ruleID != nil` block. The optionality is not a domain
  fact — the domain forbids a ruleset with zero rules. It is a JOIN artefact:
  the LEFT JOIN admits NULL rule rows that INNER JOIN would exclude. The
  catalog move is "Eliminate Phantom Optionality" — switch to INNER JOIN so
  the pointers and the nil-check block disappear together. Without the
  catalog the agent may flag "defensive code" or "nullable handling" without
  naming the move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: optionality-is-join-artefact
    text: |
      The agent identifies that the `*uuid.UUID` / `*time.Time` rule fields
      and the `if ruleID != nil` block exist because of the LEFT JOIN, not
      because the domain permits a ruleset without rules. It locates the
      cause in the query, not in the Go scan.
  - id: named-move-eliminate-phantom-optionality
    text: |
      The agent names the move it is proposing as
      "Eliminate Phantom Optionality" (or that exact catalog phrase),
      not just a generic "remove defensive code" or "tighten the type"
      observation.
---

Please review this codebase and identify any refactoring candidates worth exploring.
