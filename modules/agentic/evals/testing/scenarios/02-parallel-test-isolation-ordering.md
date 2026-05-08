---
id: 02-parallel-test-isolation-ordering
title: Refuse t.Parallel() before namespacing shared fixtures
fixture: fixtures/parallel-test-isolation
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `TestFetchUserHappyPath` and `TestFetchUserMissing` both write to and read
  from the same `stubRegistry["alice"]` key. Sequentially they pass; in
  parallel they race — one test's `delete` runs while the other is mid-read,
  or both writes interleave. The correct order is namespace the fixtures
  (per-test stub key, per-test registry, or per-test server), then remove
  the shared mutable state, then add `t.Parallel()`. A flaky test under
  parallelism is a race-condition bug; the fix is isolation, not retry.
  The prompt is deliberately narrow ("just add `t.Parallel()`") to apply
  pressure: the rule's job is to make the agent push back on the request
  rather than comply. Without the ordering rule encoded in the testing
  skill, the agent may either comply silently or refactor without naming
  the four-step ordering — both of which fail to externalize the rule the
  user wants enforced project-wide.
assertions:
  - id: refuses-parallel-before-isolation
    text: |
      The agent does NOT submit a change whose final state is
      `t.Parallel()` added to a test that still writes to the
      package-global `stubRegistry`. Either the agent leaves
      `t.Parallel()` out entirely (and explains why), or it first
      removes the shared mutable state (per-test registry, dependency
      injection, per-test key, or equivalent) and only then adds
      `t.Parallel()`. Compliance with the user's narrow request without
      addressing isolation fails this assertion.
  - id: names-four-step-ordering
    text: |
      The agent explicitly names the ordering with at least three of the
      four steps in this order: (1) namespace the test fixtures /
      isolate per-test, (2) remove shared mutable state, (3) add
      `t.Parallel()`, (4) drop `-p 1` from the test runner config. The
      ordering must be presented as a sequence ("first … then … then
      …"), not just a list of unrelated suggestions. A generic "isolate
      before parallelizing" without the numbered or sequenced steps
      fails this assertion.
  - id: flaky-is-race-bug
    text: |
      The agent's reasoning explicitly frames a flaky parallel test as
      a race-condition bug to be fixed via isolation, not as something
      to retry, skip, mark `t.Skip`, or paper over with a sleep. The
      claim must be made — "a flaky test under parallelism is a race
      bug", "fix the isolation, don't retry", or equivalent. Silent
      avoidance of retry/skip is not enough.
---

The CI pipeline is slow. A teammate asked you to add `t.Parallel()` to both
test functions in `userapi_test.go` so they run concurrently and the build
finishes faster. Make that change. Edit only files inside this fixture
directory.
