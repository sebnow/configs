package report

import "context"

type WorkflowInput struct {
	TenantID        string
	ClientID        string
	CorrelationID   string
	IssueCategoryID string
	ReportedBy      string
}

type ActivityInput struct {
	TenantID        string
	ClientID        string
	CorrelationID   string
	IssueCategoryID string
	ReportedBy      string
}

type ActivityOutput struct {
	ReportID  string
	CreatedAt int64
}

type WorkflowOutput struct {
	ReportID  string
	CreatedAt int64
}

// ProcessActivity creates the report row and returns its identifier.
// Treat this as if it were registered with a workflow engine; do not change
// its signature.
func ProcessActivity(ctx context.Context, in ActivityInput) (ActivityOutput, error) {
	// implementation lives elsewhere; not relevant to this task
	return ActivityOutput{}, nil
}

// Run is the workflow entry point. Implement the body so it invokes
// ProcessActivity with the workflow input and returns its result as a
// WorkflowOutput. Propagate any error from ProcessActivity unchanged.
func Run(ctx context.Context, in WorkflowInput) (WorkflowOutput, error) {
	// TODO: implement
	return WorkflowOutput{}, nil
}
