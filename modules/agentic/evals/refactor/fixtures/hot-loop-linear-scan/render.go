package table

import "fmt"

type Row struct {
	ID       int64
	Label    string
	UserRole string
}

type RenderedRow struct {
	ID      int64
	Label   string
	CanEdit bool
}

// RenderRows renders a slice of rows, marking each with edit permission.
// Called once per request; rows may number in the thousands.
func RenderRows(rows []Row) []RenderedRow {
	out := make([]RenderedRow, len(rows))
	for i, r := range rows {
		out[i] = RenderedRow{
			ID:      r.ID,
			Label:   r.Label,
			CanEdit: IsAdmin(r.UserRole),
		}
	}
	return out
}

// RenderHTML renders rows as an HTML table fragment.
func RenderHTML(rows []Row) string {
	result := "<table>"
	for _, r := range rows {
		editBtn := ""
		if IsAdmin(r.UserRole) {
			editBtn = fmt.Sprintf(`<a href="/edit/%d">Edit</a>`, r.ID)
		}
		result += fmt.Sprintf("<tr><td>%d</td><td>%s</td><td>%s</td></tr>",
			r.ID, r.Label, editBtn)
	}
	result += "</table>"
	return result
}
