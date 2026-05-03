package cards

// EmailCards renders cards for transactional email templates.
// Uses variant "b" for rating display and no footer (emails are not interactive).
func EmailCards(items []Card) []string {
	out := make([]string, len(items))
	for i, c := range items {
		out[i] = RenderCard(c, false, false, false, false, "b")
	}
	return out
}
