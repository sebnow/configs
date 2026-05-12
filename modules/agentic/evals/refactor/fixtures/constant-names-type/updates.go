package appeals

import (
	"time"

	"go.temporal.io/sdk/workflow"
)

// DefaultTaskQueueName is the queue every appeal workflow runs on.
const DefaultTaskQueueName = "appeals.default"

// AppealOutcomeUpdateMessageName is the workflow update name for AppealOutcomeUpdateMessage.
const AppealOutcomeUpdateMessageName = "AppealOutcomeUpdate"

type AppealOutcome string

const (
	AppealOutcomeUpheld    AppealOutcome = "upheld"
	AppealOutcomeOverruled AppealOutcome = "overruled"
)

type UserReference struct {
	ID   string
	Name string
}

type AppealOutcomeUpdateMessage struct {
	Outcome    AppealOutcome `json:"outcome"`
	ReviewedBy UserReference `json:"reviewedBy"`
	ReviewedAt time.Time     `json:"reviewedAt"`
}

// ReportEscalationUpdateMessageName is the workflow update name for ReportEscalationUpdateMessage.
const ReportEscalationUpdateMessageName = "ReportEscalationUpdate"

type ReportEscalationUpdateMessage struct {
	EscalatedTo UserReference `json:"escalatedTo"`
	Reason      string        `json:"reason"`
}

// AppealWorkflow registers the two update handlers and runs until cancelled.
func AppealWorkflow(ctx workflow.Context) error {
	if err := workflow.SetUpdateHandlerWithOptions(
		ctx,
		AppealOutcomeUpdateMessageName,
		func(ctx workflow.Context, msg AppealOutcomeUpdateMessage) error {
			return handleOutcome(ctx, msg)
		},
		workflow.UpdateHandlerOptions{},
	); err != nil {
		return err
	}

	if err := workflow.SetUpdateHandlerWithOptions(
		ctx,
		ReportEscalationUpdateMessageName,
		func(ctx workflow.Context, msg ReportEscalationUpdateMessage) error {
			return handleEscalation(ctx, msg)
		},
		workflow.UpdateHandlerOptions{},
	); err != nil {
		return err
	}

	return workflow.Await(ctx, func() bool { return false })
}

func handleOutcome(ctx workflow.Context, msg AppealOutcomeUpdateMessage) error       { return nil }
func handleEscalation(ctx workflow.Context, msg ReportEscalationUpdateMessage) error { return nil }
