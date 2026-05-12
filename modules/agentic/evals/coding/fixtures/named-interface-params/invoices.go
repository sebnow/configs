package billing

import (
	"context"
	"time"
)

// Invoice is the domain value object for a single invoice.
type Invoice struct {
	ID          string
	CustomerID  string
	AmountCents int64
	IssuedAt    time.Time
}

// Invoices is the port the billing use cases call into. Adapters live in
// the infra layer; tests use a generated mock.
type Invoices interface {
	FindByID(context.Context, string) (Invoice, error)
}
