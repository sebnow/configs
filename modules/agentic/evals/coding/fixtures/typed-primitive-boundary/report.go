package moderation

import "context"

// ReportContent records a user-submitted moderation report.
//
// reporterUserID identifies the user filing the report.
// reportedContentID identifies the piece of content being reported.
// reasonCode must be one of: "spam", "harassment", "violence".
//
// This is the public entry point of the moderation service. Callers reach it
// directly from the gRPC handler.
func ReportContent(ctx context.Context, reporterUserID, reportedContentID, reasonCode string) error {
	// TODO: validate inputs.
	// Do not implement the persistence step.
	return nil
}
