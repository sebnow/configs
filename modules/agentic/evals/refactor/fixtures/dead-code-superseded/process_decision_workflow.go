package moderation

import (
	"time"

	"go.temporal.io/sdk/workflow"
)

// ProcessDecisionWorkflow is the current entry point for applying a content
// moderation decision. Every gRPC handler and every scheduled job uses this
// workflow; it replaced ApplyDecisionWorkflow at the cutover two months ago.
func ProcessDecisionWorkflow(ctx workflow.Context, input ProcessDecisionInput) (ProcessDecisionResult, error) {
	ctx = workflow.WithActivityOptions(ctx, workflow.ActivityOptions{
		StartToCloseTimeout: 30 * time.Second,
	})

	var applied AppliedDecision
	if err := workflow.ExecuteActivity(ctx, ApplyDecisionAndAudit, input).Get(ctx, &applied); err != nil {
		return ProcessDecisionResult{}, err
	}

	if err := workflow.ExecuteActivity(ctx, NotifyDownstream, applied).Get(ctx, nil); err != nil {
		return ProcessDecisionResult{}, err
	}

	return ProcessDecisionResult{ID: applied.ID, AppliedAt: applied.AppliedAt}, nil
}

type ProcessDecisionInput struct {
	ReportID   string
	Outcome    string
	ReviewerID string
}

type ProcessDecisionResult struct {
	ID        string
	AppliedAt time.Time
}

type AppliedDecision struct {
	ID        string
	AppliedAt time.Time
}
