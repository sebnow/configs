package memberships

import "context"

type MembershipID string

type DB interface {
	BeginTx(ctx context.Context) (Tx, error)
}

type Tx interface {
	ExecContext(ctx context.Context, query string, args ...any) (Result, error)
	Commit() error
	Rollback() error
}

type Result interface {
	RowsAffected() (int64, error)
}

type RefundsRepository interface {
	ComputeRefund(ctx context.Context, id MembershipID) (int64, error)
}

type AuditRepository interface {
	// AuditRepository wraps a typed audit-log writer; unused in cancel.go
	// today because Cancel issues raw SQL to keep the fragmented-tx shape
	// visible to readers, but real call sites would use this.
	Append(ctx context.Context, subject MembershipID, action, reason string) error
}
