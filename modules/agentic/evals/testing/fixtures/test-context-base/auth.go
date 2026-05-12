package moderation

import (
	"context"
	"errors"
)

// APIKey identifies a tenant API credential.
type APIKey struct {
	ID       string
	TenantID string
}

type apiKeyKey struct{}

// WithAPIKey stashes an APIKey on the context for downstream handlers.
func WithAPIKey(ctx context.Context, key APIKey) context.Context {
	return context.WithValue(ctx, apiKeyKey{}, key)
}

// APIKeyFromContext returns the APIKey previously placed on ctx.
func APIKeyFromContext(ctx context.Context) (APIKey, bool) {
	key, ok := ctx.Value(apiKeyKey{}).(APIKey)
	return key, ok
}

// ErrMissingAPIKey is returned when the context carries no APIKey.
var ErrMissingAPIKey = errors.New("missing api key on context")

// RequireAPIKey returns the APIKey on ctx or ErrMissingAPIKey.
func RequireAPIKey(ctx context.Context) (APIKey, error) {
	key, ok := APIKeyFromContext(ctx)
	if !ok {
		return APIKey{}, ErrMissingAPIKey
	}
	return key, nil
}
