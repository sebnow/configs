package eventbus

import (
	"context"
	"time"
)

// Event is the unit of work flowing through the bus. The `Payload` field
// is an untyped bag — every producer fills in whatever keys they remember
// and every consumer re-derives the shape via type assertions.
//
// There is no compile-time check that an `OrderPlaced` event carries
// `orderId`, `amountCents`, and `currency`, nor that those fields have
// the expected types. A typo in a key returns the zero value silently.
type Event struct {
	Kind      string
	Payload   map[string]any
	Timestamp time.Time
}

// Bus is a minimal pub/sub abstraction.
type Bus interface {
	Publish(ctx context.Context, event Event) error
	Subscribe(kind string, handler func(ctx context.Context, event Event) error)
}

// PublishOrderPlaced is one of three places that produce an OrderPlaced
// event. Note `amount_cents` (snake_case) — `payments.go` writes the same
// field as `amountCents`. Both compile; only one matches the consumer's
// expectations.
func PublishOrderPlaced(ctx context.Context, bus Bus, orderID string, amountCents int64, currency string) error {
	return bus.Publish(ctx, Event{
		Kind: "order.placed",
		Payload: map[string]any{
			"orderId":      orderID,
			"amount_cents": amountCents,
			"currency":     currency,
		},
		Timestamp: time.Now(),
	})
}
