---
id: 13-discoverability-consolidate-modules
title: Discoverability - "consolidate tightly-coupled modules"
fixture: fixtures/_empty
expect_trigger: true
target_lens: null
category: discoverability
rationale: |
  Scenarios 10–14 verify that the skill activates on the five trigger
  phrases listed in the PRD's success criteria. "Consolidate tightly-coupled
  modules" is the phrasing listed verbatim in the skill's frontmatter
  description; failure here indicates a description mismatch.
assertions:
  - id: entered-refactor-process
    text: |
      The agent enters a structured refactor process:
      names friction lenses, asks friction questions,
      or proposes a codebase exploration — despite the empty fixture.
---

consolidate tightly-coupled modules
