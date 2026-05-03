package report

import (
	"encoding/json"
	"fmt"
	"time"
)

type Report struct {
	UserID    string
	Generated time.Time
	Rows      []Row
}

type Row struct {
	Label string
	Value float64
}

// FetchReport is the package entry point for generating a user report.
// It is called by internal service handlers; it is not part of a public SDK.
func FetchReport(userID string, from, to time.Time) (*Report, error) {
	q := buildQuery(userID, from, to)
	raw, err := runQuery(q)
	if err != nil {
		return nil, fmt.Errorf("report query: %w", err)
	}
	return parseReport(userID, raw)
}

type query struct {
	UserID string
	From   time.Time
	To     time.Time
}

func buildQuery(userID string, from, to time.Time) query {
	return query{UserID: userID, From: from, To: to}
}

func runQuery(q query) ([]byte, error) {
	// Stub: real implementation queries the analytics store.
	return json.Marshal(map[string]any{
		"user_id": q.UserID,
		"rows":    []any{},
	})
}

func parseReport(userID string, raw []byte) (*Report, error) {
	var envelope struct {
		Rows []struct {
			Label string  `json:"label"`
			Value float64 `json:"value"`
		} `json:"rows"`
	}
	if err := json.Unmarshal(raw, &envelope); err != nil {
		return nil, fmt.Errorf("parse report: %w", err)
	}
	rows := make([]Row, len(envelope.Rows))
	for i, r := range envelope.Rows {
		rows[i] = Row{Label: r.Label, Value: r.Value}
	}
	return &Report{
		UserID:    userID,
		Generated: time.Now(),
		Rows:      rows,
	}, nil
}
