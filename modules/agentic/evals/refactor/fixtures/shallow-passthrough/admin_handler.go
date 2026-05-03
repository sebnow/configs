package users

import (
	"context"
	"fmt"
)

type AdminHandler struct {
	repo *UserRepository
}

func NewAdminHandler(repo *UserRepository) *AdminHandler {
	return &AdminHandler{repo: repo}
}

func (h *AdminHandler) DeleteUser(ctx context.Context, id int64) error {
	return h.repo.Delete(ctx, id)
}

func (h *AdminHandler) CreateUser(ctx context.Context, email, name string) (*User, error) {
	return h.repo.Create(ctx, email, name)
}

func (h *AdminHandler) ListUser(ctx context.Context, id int64) {
	u, err := h.repo.GetByID(ctx, id)
	if err != nil {
		fmt.Printf("user %d not found: %v\n", id, err)
		return
	}
	fmt.Printf("User %d: %s <%s>\n", u.ID, u.Name, u.Email)
}
