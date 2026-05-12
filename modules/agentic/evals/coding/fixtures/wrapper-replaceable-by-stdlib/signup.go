package signup

import (
	"errors"
	"strings"
)

var ErrBadEmail = errors.New("bad email")

// validateEmail reports whether s passes the package's syntactic email
// format check. Returns nil when valid.
func validateEmail(s string) error {
	s = strings.TrimSpace(s)
	if s == "" {
		return errors.New("empty")
	}
	if len(s) > 254 {
		return errors.New("too long")
	}
	if strings.Count(s, "@") != 1 {
		return errors.New("must contain exactly one @")
	}
	parts := strings.SplitN(s, "@", 2)
	if parts[0] == "" {
		return errors.New("empty local part")
	}
	if parts[1] == "" || !strings.Contains(parts[1], ".") {
		return errors.New("empty or dotless domain")
	}
	return nil
}

type SignupRequest struct {
	Email    string
	Username string
}

// Signup validates the request email and (TODO) creates the user.
func Signup(req SignupRequest) error {
	if err := validateEmail(req.Email); err != nil {
		return ErrBadEmail
	}
	// TODO: persist user.
	return nil
}

// ChangeEmail validates the new email and (TODO) updates the user's email.
func ChangeEmail(userID, newEmail string) error {
	if err := validateEmail(newEmail); err != nil {
		return ErrBadEmail
	}
	// TODO: persist email change.
	return nil
}
