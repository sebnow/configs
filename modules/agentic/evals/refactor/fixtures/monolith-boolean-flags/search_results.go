package cards

// SearchResultCards renders compact cards for search result lists.
func SearchResultCards(items []Card) []string {
	out := make([]string, len(items))
	for i, c := range items {
		out[i] = RenderCard(c, true, false, false, false, "b")
	}
	return out
}
