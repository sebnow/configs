package eventbus

import (
	"context"
	"errors"
	"fmt"
)

// HandleOrderPlaced re-derives the OrderPlaced schema by hand.
//
// Every field access is a type assertion; every required-field rule is a
// runtime nil-check; the currency-format rule lives here even though
// `inventory_handler.go` repeats the same rule for the same event kind.
// A typo such as `Payload["orderID"]` (capital D) returns nil silently.
func HandleOrderPlaced(ctx context.Context, event Event) error {
	orderID, ok := event.Payload["orderId"].(string)
	if !ok || orderID == "" {
		return errors.New("orderId required")
	}
	amountCents, ok := event.Payload["amountCents"].(float64)
	if !ok || amountCents <= 0 {
		return errors.New("amountCents must be a positive number")
	}
	currency, ok := event.Payload["currency"].(string)
	if !ok || len(currency) != 3 {
		return errors.New("currency must be ISO-4217 (three letters)")
	}
	return persistOrder(ctx, orderID, int64(amountCents), currency)
}

func persistOrder(ctx context.Context, orderID string, amountCents int64, currency string) error {
	_ = ctx
	_ = orderID
	_ = amountCents
	_ = currency
	return nil
}

// CallSiteSweep illustrates how the same event payload is decoded
// independently by every consumer in the package.
func CallSiteSweep(ctx context.Context, event Event) {
	_ = HandleOrderPlaced(ctx, event)
	_ = ReserveStock(ctx, event)
	fmt.Println("decoded order events twice from the same payload")
}
