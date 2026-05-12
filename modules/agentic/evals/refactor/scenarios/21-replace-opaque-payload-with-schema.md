---
id: 21-replace-opaque-payload-with-schema
title: Event bus carries OrderPlaced as map[string]any with re-derived schema
fixture: fixtures/opaque-payload-event-bus
expect_trigger: true
target_lens: compression
category: pressure
rationale: |
  `Event.Payload` is a `map[string]any`. Three producers write the same
  `OrderPlaced` shape (one of them with `amount_cents` instead of
  `amountCents`, with no compile error) and two consumers re-derive the
  schema independently with type assertions like
  `event.Payload["orderId"].(string)` and `event.Payload["amountCents"].(float64)`.
  The same field-presence and currency-format rules appear in both
  consumers because no canonical type owns them. The catalog move is
  "Replace Opaque Payload with Schema" — give `OrderPlaced` a typed
  struct with the fields it actually has, push validation into a parse
  constructor, and let the compiler reject typos and producer/consumer
  drift. The `audit.LogEntry.Details` field is a counter-example: it is
  free-form debugging context with no domain semantics and no typed
  consumer, and should remain `map[string]any`. Without the catalog the
  agent may flag this as "weak typing", "untyped events", "stringly typed
  data", or "introduce a struct" without naming the move that resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: opaque-vs-free-form
    text: |
      The agent observes that `Event.Payload` carries a fixed schema by
      convention (every `OrderPlaced` has `orderId`, `amountCents`,
      `currency`) that today lives only in producer/consumer
      assertions, and that this allows producer/consumer drift such as
      `amount_cents` vs `amountCents` and silent typos in field names.
      It distinguishes this from `audit.LogEntry.Details`, which is
      genuinely free-form structured logging context with no fixed
      schema and should remain `map[string]any`.
  - id: named-move-replace-opaque-payload-with-schema
    text: |
      The agent names the move it is proposing using the verbatim catalog
      phrase "Replace Opaque Payload with Schema". A paraphrase such as
      "introduce a struct", "use typed events", "stringly typed", or
      "replace map with struct" does NOT satisfy this assertion — the
      agent must reproduce the exact catalog phrase. The proposal must
      also indicate that the typed schema enforces invariants in one
      place (e.g. a parse constructor or struct tags), not merely that a
      struct replaces the map.
---

Please review this codebase and identify any refactoring candidates worth exploring.
