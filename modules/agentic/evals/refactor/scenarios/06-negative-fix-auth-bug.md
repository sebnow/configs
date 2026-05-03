---
id: 06-negative-fix-auth-bug
title: Negative - bug fix request should not enter refactor process
fixture: fixtures/_empty
expect_trigger: false
target_lens: null
category: negative
rationale: |
  A bug-fix request is an implementation task, not an invitation to find
  architectural friction. The skill description says not to use the refactor
  skill for implementing already-decided changes. The failure mode is the
  agent framing a debugging task as Phase 1 exploration and producing a
  candidate list when the user just wants the bug fixed.
assertions:
  - id: no-phase-framing
    text: |
      The agent does not frame the response with phases
      (e.g. "Phase 1", "explore", "candidate list", "grilling").
  - id: no-numbered-candidates
    text: |
      The agent does not present a numbered list of refactoring candidates.
  - id: treated-as-bug-fix
    text: |
      The agent treats this as a debugging or coding task,
      not a refactoring engagement.
---

The authentication middleware has a bug: it does not validate the JWT expiry field,
so expired tokens are accepted. Please locate the issue and fix it.
