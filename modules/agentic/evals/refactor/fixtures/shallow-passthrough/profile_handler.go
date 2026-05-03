package users

import (
	"context"
	"fmt"
)

type ProfileHandler struct {
	repo *UserRepository
}

func NewProfileHandler(repo *UserRepository) *ProfileHandler {
	return &ProfileHandler{repo: repo}
}

func (h *ProfileHandler) ShowProfile(ctx context.Context, userID int64) {
	u, err := h.repo.GetByID(ctx, userID)
	if err != nil {
		fmt.Printf("error loading profile: %v\n", err)
		return
	}
	fmt.Printf("Profile: %s <%s>\n", u.Name, u.Email)
}

func (h *ProfileHandler) UpdateEmail(ctx context.Context, userID int64, newEmail string) error {
	u, err := h.repo.GetByID(ctx, userID)
	if err != nil {
		return err
	}
	u.Email = newEmail
	return h.repo.Update(ctx, u)
}
