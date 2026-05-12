package moderation

import (
	"context"
	"testing"
)

// TODO: extend this file to cover RequireAPIKey on both the present and
// missing branches. The scaffolding below was written quickly; match the
// repository's test conventions when you flesh it out.

func TestWithAPIKeyRoundTrip(t *testing.T) {
	ctx := context.WithValue(context.Background(), apiKeyKey{}, APIKey{ID: "k1", TenantID: "t1"})
	got, ok := APIKeyFromContext(ctx)
	if !ok {
		t.Fatal("expected key on context")
	}
	if got.ID != "k1" {
		t.Errorf("got ID %q, want %q", got.ID, "k1")
	}
}
