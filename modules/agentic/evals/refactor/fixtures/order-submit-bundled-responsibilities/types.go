package orders

import (
	"context"
	"database/sql"
)

type SubmitRequest struct {
	CustomerID    string
	CustomerEmail string
	Region        string
	Items         []LineItem
}

type LineItem struct {
	SKU      string
	Quantity int
}

type Order struct {
	ID          string
	CustomerID  string
	Subtotal    int64
	Tax         int64
	Total       int64
	CurrencyISO string
}

type RateClient interface {
	LookupUnitPrice(ctx context.Context, sku string) (int64, string, error)
	LookupTax(ctx context.Context, region string, subtotal int64) (int64, error)
}

type Bus interface {
	Publish(ctx context.Context, kind string, payload any) error
}

type EmailClient interface {
	Send(ctx context.Context, to, subject, body string) error
}

type DB interface {
	BeginTx(ctx context.Context) (*sql.Tx, error)
}
