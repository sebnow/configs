package postgres

import (
	"context"
	"database/sql"
	"time"

	"github.com/google/uuid"
)

type Ruleset struct {
	ID       uuid.UUID
	TenantID uuid.UUID
	Trigger  string
	Rules    []Rule
}

type Rule struct {
	ID         uuid.UUID
	TenantID   uuid.UUID
	Sequence   uint32
	IsTerminal bool
	CreatedAt  time.Time
}

type Repository struct {
	db *sql.DB
}

func (r *Repository) ListRulesets(ctx context.Context, tenantID uuid.UUID) ([]Ruleset, error) {
	const query = `
		SELECT
			ruleset.id, ruleset.tenant_id, ruleset.trigger,
			rule.id, rule.tenant_id, rule.sequence, rule.is_terminal, rule.created_at
		FROM ruleset
		LEFT JOIN rule ON rule.ruleset_id = ruleset.id
		WHERE ruleset.tenant_id = $1
		ORDER BY ruleset.id, rule.sequence
	`
	rows, err := r.db.QueryContext(ctx, query, tenantID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var rulesets []Ruleset
	rulesetByID := map[uuid.UUID]*Ruleset{}

	for rows.Next() {
		var rs Ruleset
		var ruleID *uuid.UUID
		var ruleTenantID *uuid.UUID
		var ruleSequence *uint32
		var ruleIsTerminal *bool
		var ruleCreatedAt *time.Time

		if err := rows.Scan(
			&rs.ID, &rs.TenantID, &rs.Trigger,
			&ruleID, &ruleTenantID, &ruleSequence, &ruleIsTerminal, &ruleCreatedAt,
		); err != nil {
			return nil, err
		}

		existing, ok := rulesetByID[rs.ID]
		if !ok {
			rulesets = append(rulesets, rs)
			existing = &rulesets[len(rulesets)-1]
			rulesetByID[rs.ID] = existing
		}

		// Add the rule if it exists (LEFT JOIN may return NULL for rulesets without rules).
		if ruleID != nil {
			rule := Rule{
				ID:       *ruleID,
				TenantID: *ruleTenantID,
				Sequence: *ruleSequence,
			}
			if ruleIsTerminal != nil {
				rule.IsTerminal = *ruleIsTerminal
			}
			if ruleCreatedAt != nil {
				rule.CreatedAt = *ruleCreatedAt
			}
			existing.Rules = append(existing.Rules, rule)
		}
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return rulesets, nil
}
