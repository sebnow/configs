package cards

// PromoCards renders sale cards with promo badges and an add-to-cart footer.
func PromoCards(items []Card) []string {
	out := make([]string, len(items))
	for i, c := range items {
		out[i] = RenderCard(c, false, false, true, true, "c")
	}
	return out
}
