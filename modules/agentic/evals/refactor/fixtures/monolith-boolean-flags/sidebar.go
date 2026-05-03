package cards

// SidebarCards renders minimal compact cards for the sidebar recommendation widget.
func SidebarCards(items []Card) []string {
	out := make([]string, len(items))
	for i, c := range items {
		out[i] = RenderCard(c, true, false, false, false, "a")
	}
	return out
}
