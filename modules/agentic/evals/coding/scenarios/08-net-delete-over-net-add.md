---
id: 08-net-delete-over-net-add
title: Custom wrapper replaceable by stdlib is deleted, not rewritten
fixture: fixtures/wrapper-replaceable-by-stdlib
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The fixture has a hand-rolled `validateEmail` function with two
  callers (`Signup` and `ChangeEmail`). The Go stdlib offers
  `net/mail.ParseAddress`, which covers the same validation — and more
  — out of the box. Two implementations both satisfy "switch to stdlib":
  rewriting `validateEmail` so its body delegates to `mail.ParseAddress`
  (keeps the wrapper, ~6 line function remains) or deleting
  `validateEmail` and inlining `mail.ParseAddress` at both call sites
  (-22 lines, +2 lines). Without a stated net-delete-over-net-add
  preference, agents tend toward the rewrite — they read "switch to
  stdlib" as "keep the local helper, change its body" and prefer that
  over duplicating the stdlib call at two call sites. With the
  principle loaded, the agent picks the deletion: the wrapper has
  nothing of value over `mail.ParseAddress` worth keeping.
assertions:
  - id: callers-use-stdlib
    text: |
      Both `Signup` and `ChangeEmail` reach `mail.ParseAddress` for
      the validation check, either by calling it directly in their
      bodies or by calling a helper whose body is a single
      `mail.ParseAddress` invocation. The hand-rolled
      `strings.Count`/`strings.SplitN`/`strings.Contains` logic is
      gone from the validation path.
  - id: wrapper-deleted
    text: |
      In the final state, `validateEmail` (or any equivalent thin
      pass-through wrapper around `mail.ParseAddress` in the `signup`
      package) is gone. The hand-rolled function is removed entirely,
      not rewritten to delegate to `mail.ParseAddress`. Both `Signup`
      and `ChangeEmail` call `mail.ParseAddress` directly. A
      genuinely-different helper that adds value beyond a pass-through
      — e.g. trimming whitespace first, or wrapping the error in a
      domain sentinel — is acceptable, but a one-line forwarder is
      not.
  - id: cites-net-delete
    text: |
      The agent justifies removing `validateEmail` (rather than
      rewriting it to delegate to `mail.ParseAddress`) by appealing
      to a deletion preference. Phrases like "net-delete over
      net-add", "the change that removes more is better", "delete
      more than we add", "prefer the smaller diff", "remove the
      indirection rather than rewrite it", or "the wrapper adds no
      value worth keeping" satisfy this assertion. Justifying purely
      on grounds unrelated to deletion size — e.g. "to use the
      stdlib idiom" or "for consistency" — does not satisfy this
      assertion.
---

Switch signup and email-change validation to use `net/mail.ParseAddress`
from the Go standard library.
