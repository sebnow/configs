package memberships

import (
	"context"
	"fmt"
)

// NotifyCanceled is a deliberate counter-example. It performs two writes —
// one to the local outbox table and one to a remote billing API — that
// cannot share a transactional boundary because the billing API is an
// external system the database knows nothing about. The right shape here
// is a saga / outbox pattern with retries and idempotency keys, not a
// single transaction. The fragmentation in this method is domain-driven:
// the boundaries are real because the systems are real.
func (s *Service) NotifyCanceled(ctx context.Context, id MembershipID, billing BillingAPI) error {
	tx, err := s.db.BeginTx(ctx)
	if err != nil {
		return fmt.Errorf("begin outbox tx: %w", err)
	}
	if _, err := tx.ExecContext(ctx,
		"INSERT INTO outbox (subject_id, kind, payload) VALUES ($1, $2, $3)",
		id, "membership.canceled", `{}`,
	); err != nil {
		_ = tx.Rollback()
		return fmt.Errorf("insert outbox: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit outbox: %w", err)
	}

	if err := billing.CancelSubscription(ctx, string(id)); err != nil {
		return fmt.Errorf("billing cancel: %w", err)
	}
	return nil
}

type BillingAPI interface {
	CancelSubscription(ctx context.Context, externalID string) error
}
