package moderation

import (
	"context"
	"net/http"
)

// HTTP handlers that re-derive validation rules for raw-string IDs every time.
// The handler reads three string fields from the request and forwards them
// positionally — if the request struct ever reorders fields, the wrong IDs
// flow through without a compile error.

type BlockUserRequest struct {
	UserID     string `json:"userId"`
	ContentID  string `json:"contentId"`
	ReporterID string `json:"reporterId"`
}

func (s *Server) HandleBlockUser(w http.ResponseWriter, r *http.Request) {
	req := s.decodeBlockUser(r)
	if err := BlockUser(r.Context(), req.UserID, req.ContentID, req.ReporterID); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

func (s *Server) HandleAssignReviewer(w http.ResponseWriter, r *http.Request) {
	reviewerID := r.URL.Query().Get("reviewerId")
	contentID := r.URL.Query().Get("contentId")
	if err := AssignReviewer(r.Context(), reviewerID, contentID); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

type Server struct{}

func (s *Server) decodeBlockUser(r *http.Request) BlockUserRequest {
	_ = r
	return BlockUserRequest{}
}

// CallSiteSweep is illustrative of the pattern across the package: every
// caller passes raw strings and the validation rules ("non-empty", "must
// match prefix v1_") live wherever the author remembered to add them.
func CallSiteSweep(ctx context.Context) {
	_ = BlockUser(ctx, "u_123", "c_456", "u_789")
	_ = AssignReviewer(ctx, "u_789", "c_456")
	_, _ = FindReportsByUser(ctx, "u_123")
	_, _ = FindReportsAgainstContent(ctx, "c_456")
}
