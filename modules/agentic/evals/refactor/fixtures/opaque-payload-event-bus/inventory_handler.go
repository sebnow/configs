package eventbus

import (
	"context"
	"errors"
)

// ReserveStock consumes the same OrderPlaced event but re-derives the
// schema with its own type assertions and its own validation rules. The
// "currency must be three letters" rule appears here in addition to
// `order_handler.go`, and the two definitions could drift the next time
// either is touched.
func ReserveStock(ctx context.Context, event Event) error {
	orderID, ok := event.Payload["orderId"].(string)
	if !ok || orderID == "" {
		return errors.New("orderId required")
	}
	amountCents, ok := event.Payload["amountCents"].(float64)
	if !ok || amountCents <= 0 {
		return errors.New("amountCents must be positive")
	}
	currency, ok := event.Payload["currency"].(string)
	if !ok || len(currency) != 3 {
		return errors.New("currency must be three letters")
	}
	return reserve(ctx, orderID, int64(amountCents), currency)
}

func reserve(ctx context.Context, orderID string, amountCents int64, currency string) error {
	_ = ctx
	_ = orderID
	_ = amountCents
	_ = currency
	return nil
}
