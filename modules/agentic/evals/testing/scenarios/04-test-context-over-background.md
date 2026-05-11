---
id: 04-test-context-over-background
title: Use t.Context() in tests, never context.Background()
fixture: fixtures/test-context-base
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `t.Context()` is the correct base context in Go test code: it is canceled
  when the test completes, so any goroutine, connection, or derived deadline
  is torn down at the test boundary. `context.Background()` does not carry
  that cancellation and produces leaks under `-race` and parallel execution.
  The fixture's existing `auth_test.go` was scaffolded with
  `context.Background()`, which makes the path of least resistance to
  replicate that pattern in the new test cases. Without the rule encoded in
  the testing skill, the agent extends the scaffolding verbatim. With the
  rule, the agent both writes new tests against `t.Context()` and rewrites
  the existing `context.Background()` usage so the convention holds across
  the file.
assertions:
  - id: uses-t-context
    text: |
      Every test function added or modified in `auth_test.go` builds its
      base context from `t.Context()` (directly, or as the parent of a
      `context.WithValue` / `context.WithTimeout` / `context.WithCancel`
      derivation). At least one test calls `t.Context()` explicitly.
  - id: no-context-background-in-tests
    text: |
      The final state of `auth_test.go` contains zero references to
      `context.Background()` and zero references to `context.TODO()`.
      Leaving the original `context.Background()` line in
      `TestWithAPIKeyRoundTrip` untouched fails this assertion — the rule
      applies to the whole test file, not just newly-added tests.
  - id: rationale-named
    text: |
      The agent's reasoning explicitly references the `t.Context()`
      convention or the cancellation-at-test-boundary motivation
      (e.g. "use `t.Context()` in tests", "canceled when the test
      finishes", "never `context.Background()` in tests"). Producing the
      right artifact silently does not satisfy this assertion.
---

Extend `auth_test.go` to cover `RequireAPIKey` on both branches: when the
context carries an `APIKey`, and when it does not. Edit only files inside
this fixture directory. Match the test conventions of this codebase when
you choose the base context.
