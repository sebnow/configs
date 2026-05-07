Go event bus whose `Event` carries a `Payload map[string]any` field. Every
publisher fills the map with arbitrary key/value pairs and every consumer
re-derives the schema with type assertions like
`payload["orderId"].(string)` and `payload["amountCents"].(float64)`. The
shape of an `OrderPlaced` event lives nowhere in the type system — readers
and writers must agree by convention, and a typo in a key is a runtime
nil-or-panic away.

`order_handler.go` and `inventory_handler.go` both consume the same event
kind and both re-derive the same field assertions independently; a third
producer in `payments.go` sometimes writes `amount_cents` (snake_case) and
sometimes `amountCents`, with no compile error to flag the drift.
Validation rules ("amount must be positive", "currency must be ISO-4217")
are sprinkled across consumers wherever the author remembered.

`audit/log_entry.go` is a deliberate counter-example: it ships a
`Details map[string]any` field that carries free-form structured logging
context — feature flags evaluated, request headers, arbitrary debugging
breadcrumbs. There is no fixed schema, no consumer-side parser, and no
domain semantics; downstream readers are humans and log indexers, not
typed code. That field should remain `map[string]any`.
