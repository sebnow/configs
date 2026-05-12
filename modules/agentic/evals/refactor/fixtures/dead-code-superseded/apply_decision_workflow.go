package moderation

import (
	"time"

	"go.temporal.io/sdk/workflow"
)

// ApplyDecisionWorkflow is the legacy decision-application workflow. It was
// superseded by ProcessDecisionWorkflow two months ago. Every caller has
// migrated. No in-flight executions remain. The file is kept around in case
// it is needed.
func ApplyDecisionWorkflow(ctx workflow.Context, input ApplyDecisionInput) (ApplyDecisionResult, error) {
	ctx = workflow.WithActivityOptions(ctx, workflow.ActivityOptions{
		StartToCloseTimeout: 30 * time.Second,
	})

	var decisionID string
	if err := workflow.ExecuteActivity(ctx, ApplyDecision, input).Get(ctx, &decisionID); err != nil {
		return ApplyDecisionResult{}, err
	}

	if err := workflow.ExecuteActivity(ctx, EmitLegacyAudit, decisionID).Get(ctx, nil); err != nil {
		return ApplyDecisionResult{}, err
	}

	if err := workflow.ExecuteActivity(ctx, NotifyDownstreamLegacy, decisionID).Get(ctx, nil); err != nil {
		return ApplyDecisionResult{}, err
	}

	return ApplyDecisionResult{ID: decisionID, AppliedAt: workflow.Now(ctx)}, nil
}

type ApplyDecisionInput struct {
	ReportID   string
	Outcome    string
	ReviewerID string
	Source     string
}

type ApplyDecisionResult struct {
	ID        string
	AppliedAt time.Time
}
