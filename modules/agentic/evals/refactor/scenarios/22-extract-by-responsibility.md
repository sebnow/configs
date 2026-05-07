---
id: 22-extract-by-responsibility
title: OrderProcessor.Submit bundles validate, price, persist, and notify
fixture: fixtures/order-submit-bundled-responsibilities
expect_trigger: true
target_lens: shallow-modules
category: pressure
rationale: |
  `OrderProcessor.Submit` packs four independent jobs into one method body:
  request validation, line-item pricing + tax lookup, transactional
  persistence, and post-commit notification (event bus + confirmation
  email). Each block has its own collaborator (`RateClient`, `DB`, `Bus`,
  `EmailClient`), its own failure modes, and its own reasons to change.
  The compression test does not save the method — nothing is repeated;
  these are four distinct responsibilities co-located by accident of the
  HTTP entry point. The catalog move is "Extract by Responsibility" —
  split when each piece has one nameable responsibility, e.g. a
  `SubmitValidator`, a `Pricer`, an `OrderRepository`, and an
  `OrderNotifier`. The `applyDiscounts` function in `discounts.go` is a
  deliberate counter-example: it is long because coupon stacking has many
  cases, but every branch contributes to a single nameable outcome and
  splitting on length alone would just relocate the switch. Without the
  catalog the agent may flag this as "this method is too long", "violates
  SRP", "does too much", or "split into helpers" without naming the
  specific move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents a numbered list of refactoring candidates
      and asks which to explore further before proposing changes.
  - id: nameable-pieces-vs-just-long
    text: |
      The agent observes that `Submit` packs multiple distinct
      responsibilities — at minimum naming validation/pricing/persistence
      and at least one of notification/event-publishing/email — each with
      a name a reader could give to a separate type or function. It
      distinguishes this from `applyDiscounts` in `discounts.go`, which
      is long but has a single nameable purpose (apply coupons → return
      discounted subtotal) and should NOT be extracted on length grounds.
  - id: named-move-extract-by-responsibility
    text: |
      The agent names the move it is proposing using the verbatim catalog
      phrase "Extract by Responsibility". A paraphrase such as "split the
      method", "apply SRP", "break it up", "extract helpers", or "extract
      method" does NOT satisfy this assertion — the agent must reproduce
      the exact catalog phrase. The proposal must also indicate that the
      split is justified by each piece having its own nameable
      responsibility, not by the method's length alone.
---

Please review this codebase and identify any refactoring candidates worth exploring.
