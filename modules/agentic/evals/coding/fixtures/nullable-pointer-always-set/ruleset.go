package ruleset

import "context"

// Ruleset is loaded by LoadActiveRulesets. Every Ruleset returned from that
// loader is guaranteed (by a Postgres constraint added two migrations ago)
// to have a rule attached: RuleID, RuleSequence, and RuleIsTerminal are
// never nil for an active ruleset. The pointer types are a leftover from
// the historical LEFT JOIN-based loader.
type Ruleset struct {
	ID       string
	TenantID string

	RuleID         *string
	RuleSequence   *uint32
	RuleIsTerminal *bool
}

// Event is a moderation event we want to test rulesets against.
type Event struct {
	UserID  string
	Content string
}

// LoadActiveRulesets returns every active ruleset for a tenant. Active
// rulesets always have a rule (DB constraint).
func LoadActiveRulesets(ctx context.Context, tenantID string) ([]Ruleset, error) {
	// implementation omitted — Postgres-backed loader.
	return nil, nil
}

// Apply runs the ruleset's rule against the event and reports whether the
// rule terminated processing.
//
// Every caller reaches Apply via LoadActiveRulesets, so the pointer fields
// on rs are never nil here.
//
// TODO: implement.
func Apply(rs Ruleset, ev Event) (terminated bool) {
	// TODO
	return false
}
