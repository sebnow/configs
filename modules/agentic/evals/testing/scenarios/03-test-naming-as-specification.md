---
id: 03-test-naming-as-specification
title: Name tests as Given/When/Then behavior statements
fixture: fixtures/test-naming-specification
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `ApplyDiscount` has three distinct behaviors (valid input reduces price,
  negative price errors, percent outside [0, 100] errors). Each behavior
  deserves a test whose name reads as a specification — concretely, the
  `TestGiven<context>When<action>Then<outcome>` pattern, so that
  `go test -v` output reads like a behavior list. Without the naming rule
  encoded in the testing skill, the path of least resistance is a single
  `TestApplyDiscount` with table-driven subtest names like "negative price"
  or "happy path"; those compress the spec into shorthand the reader has
  to expand. With the rule, the agent surfaces each behavior in its name
  ("TestGivenNegativePriceWhenApplyDiscountThenReturnsError" or the same
  pattern as a subtest name).
assertions:
  - id: uses-given-when-then-pattern
    text: |
      At least two test functions or subtest names follow the
      `Given<context>When<action>Then<outcome>` pattern. The exact
      casing may vary (`TestGivenNegativePriceWhenApplyDiscountThenReturnsError`,
      `t.Run("Given negative price when ApplyDiscount then returns error", …)`,
      `Test_Given_NegativePrice_When_ApplyDiscount_Then_ReturnsError`), but
      all three clauses must be present in the name in that order. A
      single test with a table of `name: "negative price"` rows fails this
      assertion because none of those row names contain the
      Given/When/Then clauses.
  - id: behavior-per-name
    text: |
      The set of test or subtest names covers the three distinct behaviors
      individually — happy path, negative price rejection, out-of-range
      percent rejection — with one name per behavior. Names like
      "happy path", "error", "invalid input" that lump multiple behaviors
      under a single label fail this assertion.
  - id: rationale-named
    text: |
      The agent's reasoning explicitly references the naming-as-specification
      rule (e.g. "Given/When/Then", "name documents the behavior",
      "test name reads as a spec"). Merely producing the right artifact
      without explaining the choice does not satisfy this assertion.
---

Add a Go test file for `ApplyDiscount` in `pricing.go`. Cover the happy path
and both error paths. The names of the tests should make the package's
behavior readable from `go test -v` output without needing to open the
test file.
