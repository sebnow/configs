package moderation

import (
	"context"
	"time"
)

// APIKey is an issued API credential for a tenant.
type APIKey struct {
	ID        string
	TenantID  string
	Hash      []byte
	IssuedAt  time.Time
	RevokedAt *time.Time
}

// APIKeyRepository persists API keys.
//
// The production implementation lives in infra/postgres and is exercised by
// integration tests. At the activity layer the repository is substituted so
// the activity logic can be tested without standing up a database.
type APIKeyRepository interface {
	Create(ctx context.Context, key APIKey) error
	GetByID(ctx context.Context, id string) (APIKey, error)
	Revoke(ctx context.Context, id string, at time.Time) error
}

// IssueAPIKey is the activity that records a freshly minted API key.
//
// It is a thin wrapper around APIKeyRepository.Create today. Future revisions
// will add audit logging and enforce a per-tenant key cap, but the contract of
// returning the repository's error unchanged must be preserved.
func IssueAPIKey(ctx context.Context, repo APIKeyRepository, key APIKey) error {
	return repo.Create(ctx, key)
}
