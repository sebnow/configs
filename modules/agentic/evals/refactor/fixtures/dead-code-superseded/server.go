package moderation

import (
	"context"

	"go.temporal.io/sdk/client"
)

// Server hosts the gRPC handlers. Every handler now uses
// ProcessDecisionWorkflow; ApplyDecisionWorkflow is never executed from any
// handler in this file or anywhere else in the repository.
type Server struct {
	temporal client.Client
}

func (s *Server) SubmitDecision(ctx context.Context, req SubmitDecisionRequest) (SubmitDecisionResponse, error) {
	run, err := s.temporal.ExecuteWorkflow(ctx,
		client.StartWorkflowOptions{TaskQueue: "moderation"},
		ProcessDecisionWorkflow,
		ProcessDecisionInput{
			ReportID:   req.ReportID,
			Outcome:    req.Outcome,
			ReviewerID: req.ReviewerID,
		},
	)
	if err != nil {
		return SubmitDecisionResponse{}, err
	}
	var result ProcessDecisionResult
	if err := run.Get(ctx, &result); err != nil {
		return SubmitDecisionResponse{}, err
	}
	return SubmitDecisionResponse{ID: result.ID}, nil
}

type SubmitDecisionRequest struct {
	ReportID   string
	Outcome    string
	ReviewerID string
}

type SubmitDecisionResponse struct {
	ID string
}
