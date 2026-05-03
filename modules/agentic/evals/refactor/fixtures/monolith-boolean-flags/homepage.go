package cards

// HomepageCards renders the featured cards shown on the homepage hero section.
func HomepageCards(items []Card) []string {
	out := make([]string, len(items))
	for i, c := range items {
		out[i] = RenderCard(c, false, true, false, true, "a")
	}
	return out
}
