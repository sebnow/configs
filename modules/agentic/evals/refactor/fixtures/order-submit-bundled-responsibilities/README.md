Go order-submission service whose `OrderProcessor.Submit` packs several
independent jobs into one method body: input validation, line-item
pricing and tax lookup, transactional persistence, and post-commit
fan-out (event bus + confirmation email). Each block has its own
collaborator (`RateClient`, `DB`, `Bus`, `EmailClient`), its own failure
modes, and its own reasons to change — billing rules change tax math,
the schema team changes the inserts, marketing changes the email copy.
Today every change to any one of those touches `Submit` and risks the
others.

Nothing in `Submit` is repeated logic — these are distinct jobs
co-located by accident of the HTTP entry point. Deleting `Submit`
concentrates complexity into a new entry point rather than across many
existing callers, so the bundling earns no leverage.

`applyDiscounts` in `discounts.go` is a deliberate counter-example: it
is long because coupon stacking has many cases, but every branch
contributes to a single nameable outcome (apply coupons → return
discounted subtotal). There is no second job hidden inside it and it
should remain a single function.
