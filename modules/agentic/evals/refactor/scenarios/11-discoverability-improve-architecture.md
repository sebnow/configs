---
id: 11-discoverability-improve-architecture
title: Discoverability - "improve the architecture"
fixture: fixtures/_empty
expect_trigger: true
target_lens: null
category: discoverability
rationale: |
  Scenarios 10–14 verify that the skill activates on the five trigger
  phrases listed in the PRD's success criteria. This phrase tests a common
  architectural framing that should activate the refactor process even
  without a specific problem statement.
assertions:
  - id: entered-refactor-process
    text: |
      The agent enters a structured refactor process:
      names friction lenses, asks friction questions,
      or proposes a codebase exploration — despite the empty fixture.
---

improve the architecture
