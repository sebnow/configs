package moderation

import (
	"context"
	"errors"
	"fmt"
)

// BlockUser bans userID for the offending content reported by reporterID.
//
// userID, contentID, and reporterID are all carried as raw strings. Nothing
// in the type system prevents a caller from passing them in the wrong order;
// every call site must remember the positional convention.
func BlockUser(ctx context.Context, userID, contentID string, reporterID string) error {
	if userID == "" {
		return errors.New("userID required")
	}
	if contentID == "" {
		return errors.New("contentID required")
	}
	if reporterID == "" {
		return errors.New("reporterID required")
	}
	return persistBlock(ctx, userID, contentID, reporterID)
}

// AssignReviewer hands contentID to a moderator. The reviewer is also a user,
// but identified separately as reviewerID so the reviewer's own content is
// never moderated by this call.
func AssignReviewer(ctx context.Context, reviewerID, contentID string) error {
	if reviewerID == "" {
		return errors.New("reviewerID required")
	}
	if contentID == "" {
		return errors.New("contentID required")
	}
	return persistAssignment(ctx, reviewerID, contentID)
}

// FindReportsByUser returns all reports a user has filed.
func FindReportsByUser(ctx context.Context, userID string) ([]Report, error) {
	return loadReportsByUser(ctx, userID)
}

// FindReportsAgainstContent returns all reports filed against a piece of content.
func FindReportsAgainstContent(ctx context.Context, contentID string) ([]Report, error) {
	return loadReportsAgainstContent(ctx, contentID)
}

// Report is the persisted record of a moderation report.
type Report struct {
	ReporterID string
	UserID     string
	ContentID  string
	Reason     string
}

func persistBlock(ctx context.Context, userID, contentID, reporterID string) error {
	return execModerationStmt(ctx, fmt.Sprintf("block %s on %s by %s", userID, contentID, reporterID))
}

func persistAssignment(ctx context.Context, reviewerID, contentID string) error {
	return execModerationStmt(ctx, fmt.Sprintf("assign %s to %s", reviewerID, contentID))
}

func loadReportsByUser(ctx context.Context, userID string) ([]Report, error) {
	_ = userID
	return nil, nil
}

func loadReportsAgainstContent(ctx context.Context, contentID string) ([]Report, error) {
	_ = contentID
	return nil, nil
}

func execModerationStmt(ctx context.Context, stmt string) error {
	_ = ctx
	_ = stmt
	return nil
}
