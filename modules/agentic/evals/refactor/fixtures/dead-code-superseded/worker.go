package moderation

import (
	"go.temporal.io/sdk/worker"
)

// RegisterWorkflows registers every workflow this worker can run. Both
// workflows are still registered even though only ProcessDecisionWorkflow
// is invoked anywhere in the codebase.
func RegisterWorkflows(w worker.Worker) {
	w.RegisterWorkflow(ProcessDecisionWorkflow)
	w.RegisterActivity(ApplyDecisionAndAudit)
	w.RegisterActivity(NotifyDownstream)

	// Legacy registrations. ApplyDecisionWorkflow has no callers; no gRPC
	// handler, scheduler, or test invokes it. The activities below are
	// only reachable from the legacy workflow.
	w.RegisterWorkflow(ApplyDecisionWorkflow)
	w.RegisterActivity(ApplyDecision)
	w.RegisterActivity(EmitLegacyAudit)
	w.RegisterActivity(NotifyDownstreamLegacy)
}
