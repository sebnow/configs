package userapi

import (
	"context"
	"net/http"

	"example.com/userapi/pb"
)

type UserRepository interface {
	FindByID(ctx context.Context, id string) (*User, error)
}

type Handler struct {
	repo UserRepository
}

func NewHandler(repo UserRepository) *Handler {
	return &Handler{repo: repo}
}

func (h *Handler) GetUser(w http.ResponseWriter, r *http.Request) {
	id := r.URL.Query().Get("id")
	user, err := h.repo.FindByID(r.Context(), id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	resp := &pb.UserResponse{
		Id:          user.ID,
		DisplayName: user.DisplayName,
		Email:       user.Email,
	}
	writeProto(w, resp)
}

type User struct {
	ID          string
	DisplayName string
	Email       string
}

func writeProto(w http.ResponseWriter, m any) {
	w.Header().Set("Content-Type", "application/x-protobuf")
	w.WriteHeader(http.StatusOK)
}
