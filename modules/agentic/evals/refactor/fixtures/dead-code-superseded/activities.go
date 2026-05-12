package moderation

import "context"

// ApplyDecisionAndAudit is the activity used by the current
// ProcessDecisionWorkflow. It writes the decision and audit log in one
// transaction.
func ApplyDecisionAndAudit(ctx context.Context, input ProcessDecisionInput) (AppliedDecision, error) {
	return AppliedDecision{}, nil
}

func NotifyDownstream(ctx context.Context, applied AppliedDecision) error {
	return nil
}

// ApplyDecision is exclusive to the legacy ApplyDecisionWorkflow. No other
// caller exists.
func ApplyDecision(ctx context.Context, input ApplyDecisionInput) (string, error) {
	return "", nil
}

// EmitLegacyAudit is exclusive to the legacy workflow.
func EmitLegacyAudit(ctx context.Context, decisionID string) error {
	return nil
}

// NotifyDownstreamLegacy is exclusive to the legacy workflow.
func NotifyDownstreamLegacy(ctx context.Context, decisionID string) error {
	return nil
}
