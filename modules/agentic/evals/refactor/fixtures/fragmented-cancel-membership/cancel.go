package memberships

import (
	"context"
	"fmt"
	"log"
)

// Service.Cancel performs a single domain action — "cancel a membership" —
// that the business expects to commit-or-fail as one outcome: the membership
// state, the refund row, and the audit-log entry must all land together or
// none of them. Today the implementation fragments that outcome across three
// independent database transactions, so a process crash, network blip, or
// transient SQL error between any two steps leaves the system in a partial
// state that operators have to clean up by hand: cancellations with no
// refund row, refunds with no audit trail, audit entries that disagree with
// the membership state.
//
// The collaborators (db, refunds, audit) all live behind the same database
// handle — there is no external API call, no fan-out to a different store,
// and no reason the writes cannot share one transaction. The fragmentation
// is structural, not domain-driven.
type Service struct {
	db      DB
	refunds RefundsRepository
	audit   AuditRepository
}

func NewService(db DB, refunds RefundsRepository, audit AuditRepository) *Service {
	return &Service{db: db, refunds: refunds, audit: audit}
}

func (s *Service) Cancel(ctx context.Context, id MembershipID, reason string) error {
	// --- Step 1: own transaction — flip the membership state ---
	tx1, err := s.db.BeginTx(ctx)
	if err != nil {
		return fmt.Errorf("begin tx (state): %w", err)
	}
	if _, err := tx1.ExecContext(ctx,
		"UPDATE memberships SET status = 'canceled', canceled_at = now() WHERE id = $1",
		id,
	); err != nil {
		_ = tx1.Rollback()
		return fmt.Errorf("update membership: %w", err)
	}
	if err := tx1.Commit(); err != nil {
		return fmt.Errorf("commit state: %w", err)
	}

	// --- Step 2: own transaction — write the refund row ---
	// If this fails, the membership is already canceled but no refund exists.
	// Operators discover the gap only when the customer complains.
	tx2, err := s.db.BeginTx(ctx)
	if err != nil {
		return fmt.Errorf("begin tx (refund): %w", err)
	}
	refundCents, err := s.refunds.ComputeRefund(ctx, id)
	if err != nil {
		_ = tx2.Rollback()
		return fmt.Errorf("compute refund: %w", err)
	}
	if _, err := tx2.ExecContext(ctx,
		"INSERT INTO refunds (membership_id, amount_cents, reason) VALUES ($1, $2, $3)",
		id, refundCents, reason,
	); err != nil {
		_ = tx2.Rollback()
		return fmt.Errorf("insert refund: %w", err)
	}
	if err := tx2.Commit(); err != nil {
		return fmt.Errorf("commit refund: %w", err)
	}

	// --- Step 3: own transaction — append the audit entry ---
	// If this fails, the cancel + refund landed but the audit log is missing,
	// so the compliance report shows a refund with no recorded justification.
	tx3, err := s.db.BeginTx(ctx)
	if err != nil {
		return fmt.Errorf("begin tx (audit): %w", err)
	}
	if _, err := tx3.ExecContext(ctx,
		"INSERT INTO audit_log (subject_id, action, reason, occurred_at) VALUES ($1, $2, $3, now())",
		id, "membership.canceled", reason,
	); err != nil {
		_ = tx3.Rollback()
		return fmt.Errorf("insert audit: %w", err)
	}
	if err := tx3.Commit(); err != nil {
		return fmt.Errorf("commit audit: %w", err)
	}

	log.Printf("info: membership %s canceled", id)
	return nil
}
