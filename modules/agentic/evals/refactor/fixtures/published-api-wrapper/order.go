package order

import (
	"errors"
	"fmt"
)

type Order struct {
	ID         string
	CustomerID string
	Amount     float64
	Items      []string
}

type Result struct {
	OrderID       string
	ChargeID      string
	Notifications []string
}

// ProcessOrder validates, charges, and notifies for an order.
// This is the sole exported entry point for the order package.
// It is part of the public SDK used by external integrators.
func ProcessOrder(o Order) (*Result, error) {
	if err := validate(o); err != nil {
		return nil, fmt.Errorf("invalid order: %w", err)
	}
	chargeID, err := charge(o)
	if err != nil {
		return nil, fmt.Errorf("charge failed: %w", err)
	}
	notes, err := notify(o, chargeID)
	if err != nil {
		return nil, fmt.Errorf("notification failed: %w", err)
	}
	return &Result{
		OrderID:       o.ID,
		ChargeID:      chargeID,
		Notifications: notes,
	}, nil
}

func validate(o Order) error {
	if o.ID == "" {
		return errors.New("order ID required")
	}
	if o.CustomerID == "" {
		return errors.New("customer ID required")
	}
	if o.Amount <= 0 {
		return errors.New("amount must be positive")
	}
	if len(o.Items) == 0 {
		return errors.New("order must contain at least one item")
	}
	return nil
}

func charge(o Order) (string, error) {
	// Stub: real implementation calls payment provider.
	return fmt.Sprintf("ch_%s", o.ID), nil
}

func notify(o Order, chargeID string) ([]string, error) {
	// Stub: real implementation sends email and webhook.
	return []string{
		fmt.Sprintf("email:customer:%s", o.CustomerID),
		fmt.Sprintf("webhook:order:%s:charged:%s", o.ID, chargeID),
	}, nil
}
