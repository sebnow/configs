package userapi

import (
	"context"
	"fmt"
)

// User is a record returned by FetchUser.
type User struct {
	ID   string
	Name string
}

// stubRegistry is the package-global stub used by the tests in this package
// instead of a real upstream call. Each test installs canned responses for
// the IDs it intends to read, then asserts on what FetchUser returns.
//
// The production wiring replaces this lookup with an HTTP client; the global
// only exists for the test path.
var stubRegistry = map[string]User{}

// FetchUser returns the user with the given id from the configured backend.
//
// In tests, the backend is the package-global stubRegistry. The function
// returns an error when the id is not present.
func FetchUser(ctx context.Context, id string) (User, error) {
	u, ok := stubRegistry[id]
	if !ok {
		return User{}, fmt.Errorf("user %q not found", id)
	}
	return u, nil
}
