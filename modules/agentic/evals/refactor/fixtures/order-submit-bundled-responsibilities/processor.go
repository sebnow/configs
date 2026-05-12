package orders

import (
	"context"
	"errors"
	"fmt"
	"log"
	"strings"
)

// OrderProcessor.Submit bundles four independent responsibilities into one
// method: input validation, line-item pricing + tax lookup, transactional
// persistence, and post-commit notification. Each block has its own nameable
// purpose, its own collaborator set, its own failure modes, and its own
// reasons to change. The compression test does not save it: nothing here is
// repeated logic — it is four distinct jobs co-located by accident of the
// HTTP entry point.
type OrderProcessor struct {
	db    DB
	rates RateClient
	bus   Bus
	smtp  EmailClient
}

func NewOrderProcessor(db DB, rates RateClient, bus Bus, smtp EmailClient) *OrderProcessor {
	return &OrderProcessor{db: db, rates: rates, bus: bus, smtp: smtp}
}

func (p *OrderProcessor) Submit(ctx context.Context, req SubmitRequest) (Order, error) {
	// --- Responsibility 1: input validation ---
	if req.CustomerID == "" {
		return Order{}, errors.New("customerID required")
	}
	if !strings.Contains(req.CustomerEmail, "@") {
		return Order{}, errors.New("customerEmail must look like an address")
	}
	if req.Region == "" {
		return Order{}, errors.New("region required")
	}
	if len(req.Items) == 0 {
		return Order{}, errors.New("at least one item required")
	}
	for i, item := range req.Items {
		if item.SKU == "" {
			return Order{}, fmt.Errorf("items[%d].sku required", i)
		}
		if item.Quantity <= 0 {
			return Order{}, fmt.Errorf("items[%d].quantity must be positive", i)
		}
	}

	// --- Responsibility 2: pricing + tax ---
	var subtotal int64
	currency := ""
	for _, item := range req.Items {
		unitPrice, lineCurrency, err := p.rates.LookupUnitPrice(ctx, item.SKU)
		if err != nil {
			return Order{}, fmt.Errorf("price lookup for %s: %w", item.SKU, err)
		}
		if currency == "" {
			currency = lineCurrency
		} else if currency != lineCurrency {
			return Order{}, errors.New("mixed-currency basket not supported")
		}
		subtotal += unitPrice * int64(item.Quantity)
	}
	tax, err := p.rates.LookupTax(ctx, req.Region, subtotal)
	if err != nil {
		return Order{}, fmt.Errorf("tax lookup: %w", err)
	}
	total := subtotal + tax

	// --- Responsibility 3: persistence ---
	tx, err := p.db.BeginTx(ctx)
	if err != nil {
		return Order{}, fmt.Errorf("begin tx: %w", err)
	}
	orderID := fmt.Sprintf("ord_%s_%d", req.CustomerID, total)
	if _, err := tx.ExecContext(ctx,
		"INSERT INTO orders (id, customer_id, subtotal, tax, total, currency) VALUES ($1,$2,$3,$4,$5,$6)",
		orderID, req.CustomerID, subtotal, tax, total, currency,
	); err != nil {
		_ = tx.Rollback()
		return Order{}, fmt.Errorf("insert order: %w", err)
	}
	for _, item := range req.Items {
		if _, err := tx.ExecContext(ctx,
			"INSERT INTO order_items (order_id, sku, quantity) VALUES ($1,$2,$3)",
			orderID, item.SKU, item.Quantity,
		); err != nil {
			_ = tx.Rollback()
			return Order{}, fmt.Errorf("insert item: %w", err)
		}
	}
	if err := tx.Commit(); err != nil {
		return Order{}, fmt.Errorf("commit: %w", err)
	}

	order := Order{
		ID:          orderID,
		CustomerID:  req.CustomerID,
		Subtotal:    subtotal,
		Tax:         tax,
		Total:       total,
		CurrencyISO: currency,
	}

	// --- Responsibility 4: post-commit notification ---
	if err := p.bus.Publish(ctx, "order.placed", order); err != nil {
		log.Printf("warn: publish order.placed: %v", err)
	}
	body := fmt.Sprintf(
		"Hi! Your order %s is confirmed. Total: %d %s.",
		order.ID, order.Total, order.CurrencyISO,
	)
	if err := p.smtp.Send(ctx, req.CustomerEmail, "Order confirmed", body); err != nil {
		log.Printf("warn: send confirmation email: %v", err)
	}

	return order, nil
}
