---
id: 07-errors-classified-consumer
title: Errors are classified by what the consumer does with them
fixture: fixtures/error-audience-job-runner
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `ProcessJob` is called by a cron runner that decides what to do next based
  on the returned error: retry, drop, or escalate. The doc comment describes
  the consumer's branching explicitly. The default reflex is to wrap with
  `fmt.Errorf("send: %w", err)` and return — which is fine for inspection but
  leaves the runner unable to discriminate. The next reflex is to introduce
  sentinels named after where the failure happened (`ErrSendFailed`,
  `ErrValidateFailed`) — that classifies by source, not by what the consumer
  must do. The audience-shaped design names sentinels by consumer reaction
  (`ErrTransient`, `ErrInvalidInput`, or equivalent) so the runner branch
  reads as `errors.Is(err, jobrunner.ErrTransient)` and the meaning is
  obvious at the call site. Without the errors-classified-for-consumer-
  audience rule encoded in the coding skill, agents tend to either omit
  sentinels entirely or name them by source, leaving the consumer with no
  cleaner discrimination than a string match on the wrapped error.
assertions:
  - id: discriminable-sentinels
    text: |
      The agent introduces at least two exported sentinel errors (or typed
      error values) in the `jobrunner` package that `ProcessJob` returns,
      such that the runner can discriminate the failure modes via
      `errors.Is` or `errors.As`. A single returned error type that
      collapses every failure path, or returning unwrapped `fmt.Errorf`
      strings with no sentinel, does not satisfy this assertion.
  - id: named-by-consumer-action
    text: |
      The sentinel names (or the type names) describe the consumer's
      reaction, not the source of the failure. Names like `ErrTransient`,
      `ErrRetryable`, `ErrInvalidInput`, `ErrDropJob`, `ErrPermanent`,
      `ErrEscalate`, or semantically-equivalent variants satisfy this
      assertion. Names that classify by where the failure happened —
      `ErrSendFailed`, `ErrValidateFailed`, `ErrSenderError`,
      `ErrDownstream`, `ErrTimeout` (the source, not the action) — do
      not satisfy this assertion. If the agent uses both, the
      consumer-action names must be the ones the runner discriminates
      against; source-named errors used only as internal wrappers do not
      disqualify the answer.
  - id: cites-audience-rule
    text: |
      The agent justifies the error design by referencing the consumer
      audience or what the consumer does with the error. Phrases like
      "the runner reads these", "design the error for who consumes it",
      "errors are classified for the consumer audience", "an error is
      data with an audience", "shape the error around the consumer's
      decision", or naming the cron runner explicitly as the audience
      all satisfy this assertion. Justifying the design purely as
      "good error handling", "use sentinel errors", or "wrap with %w"
      without naming the audience or the consumer's branching does not
      satisfy this assertion.
---

Implement `ProcessJob` in `processor.go`. The function must validate that
`job.Payload` is non-empty and then call `sender.Send`. Make any package-level
additions you need so the runner can act on the returned error as the doc
comment describes.
