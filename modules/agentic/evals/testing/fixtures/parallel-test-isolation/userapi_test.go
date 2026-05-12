package userapi

import (
	"errors"
	"testing"
)

func TestFetchUserHappyPath(t *testing.T) {
	stubRegistry["alice"] = User{ID: "alice", Name: "Alice"}
	defer delete(stubRegistry, "alice")

	got, err := FetchUser(t.Context(), "alice")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got.Name != "Alice" {
		t.Errorf("got name %q, want %q", got.Name, "Alice")
	}
}

func TestFetchUserMissing(t *testing.T) {
	stubRegistry["alice"] = User{ID: "alice", Name: "Alice"}
	defer delete(stubRegistry, "alice")

	_, err := FetchUser(t.Context(), "bob")
	if err == nil {
		t.Fatal("expected error for missing user, got nil")
	}
	if errors.Is(err, nil) {
		t.Fatal("error should not be nil")
	}
}
