package audit

import "time"

// LogEntry intentionally carries free-form structured details. Producers
// attach arbitrary debugging context — evaluated feature flags, request
// headers, ad-hoc breadcrumbs — and consumers are humans reading logs or
// a log-indexing pipeline. There is no fixed schema, no domain semantics,
// and no typed consumer that decodes individual fields.
//
// Counter-example: this is the kind of payload that legitimately remains
// `map[string]any`. A typed schema would force every producer to commit
// to fields they do not yet know they will need.
type LogEntry struct {
	Timestamp time.Time
	Level     string
	Message   string
	Details   map[string]any
}

func New(level, message string, details map[string]any) LogEntry {
	return LogEntry{
		Timestamp: time.Now(),
		Level:     level,
		Message:   message,
		Details:   details,
	}
}
