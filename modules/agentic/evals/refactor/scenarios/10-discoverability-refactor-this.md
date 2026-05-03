---
id: 10-discoverability-refactor-this
title: Discoverability - "refactor this"
fixture: fixtures/_empty
expect_trigger: true
target_lens: null
category: discoverability
rationale: |
  Scenarios 10–14 verify that the skill activates on the five trigger
  phrases listed in the PRD's success criteria. The skill is only useful if
  it reliably recognises real user phrasings; each scenario tests one phrase
  against an empty fixture so activation depends entirely on the prompt, not
  the codebase. This scenario tests the shortest possible trigger.
assertions:
  - id: entered-refactor-process
    text: |
      The agent enters a structured refactor process:
      names friction lenses, asks friction questions,
      or proposes a codebase exploration — despite the empty fixture.
---

refactor this
