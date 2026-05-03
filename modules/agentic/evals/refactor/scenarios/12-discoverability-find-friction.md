---
id: 12-discoverability-find-friction
title: Discoverability - "find the friction points"
fixture: fixtures/_empty
expect_trigger: true
target_lens: null
category: discoverability
rationale: |
  Scenarios 10–14 verify that the skill activates on the five trigger
  phrases listed in the PRD's success criteria. "Find the friction points"
  uses the skill's own vocabulary; it should be the most reliably recognised
  phrase and acts as a sanity check for the description's keyword coverage.
assertions:
  - id: entered-refactor-process
    text: |
      The agent enters a structured refactor process:
      names friction lenses, asks friction questions,
      or proposes a codebase exploration — despite the empty fixture.
---

find the friction points
