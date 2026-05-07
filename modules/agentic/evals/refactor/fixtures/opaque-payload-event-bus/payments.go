package eventbus

import (
	"context"
	"time"
)

// PublishFromPaymentGateway is the third producer of OrderPlaced. It
// writes `amount_cents` (snake_case) — the consumers in
// `order_handler.go` and `inventory_handler.go` both look up
// `amountCents` (camelCase). The mismatch compiles; consumers see the
// zero value at runtime and reject the event.
func PublishFromPaymentGateway(ctx context.Context, bus Bus, orderID string, amountCents int64, currency string) error {
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
