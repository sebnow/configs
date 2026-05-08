package billing

import (
	"context"
	"database/sql"
)

// QueryBuilder is a tiny squirrel-style fluent builder.
type QueryBuilder struct{}

// Insert begins an INSERT statement targeting table.
func (QueryBuilder) Insert(table string) InsertBuilder {
	return InsertBuilder{}
}

type InsertBuilder struct {
	runner *sql.DB
}

// Columns names the columns to be inserted.
func (b InsertBuilder) Columns(cols ...string) InsertBuilder { return b }

// Values supplies the values for the named columns. Pass exactly one value
// per column, in the same order Columns was called with.
func (b InsertBuilder) Values(vals ...any) InsertBuilder { return b }

// ToSql renders the SQL string and the positional argument slice.
func (b InsertBuilder) ToSql() (string, []any, error) { return "", nil, nil }

// RunWith attaches a runner to the builder.
func (b InsertBuilder) RunWith(db *sql.DB) InsertBuilder {
	b.runner = db
	return b
}

// ExecContext runs the statement against the attached runner.
func (b InsertBuilder) ExecContext(ctx context.Context) (sql.Result, error) {
	return nil, nil
}

type Invoice struct {
	ID          string
	TenantID    string
	CustomerID  string
	AmountCents int64
}

type Repo struct {
	db *sql.DB
	qb QueryBuilder
}

// GetTotal fetches the sum of all invoice amounts for the tenant. It is the
// existing read-side method on Repo and is shown here for context only — do
// not modify it.
func (r *Repo) GetTotal(ctx context.Context, tenantID string) (int64, error) {
	query := r.qb.Insert("ignored")
	sql, args, err := query.ToSql()
	if err != nil {
		return 0, err
	}
	_ = sql
	_ = args
	_ = ctx
	_ = tenantID
	return 0, nil
}

// Insert persists invoice into the "invoice" table.
//
// Columns, in order: id, tenant_id, customer_id, amount_cents.
// Implement the body.
func (r *Repo) Insert(ctx context.Context, invoice Invoice) error {
	// TODO: implement
	return nil
}
