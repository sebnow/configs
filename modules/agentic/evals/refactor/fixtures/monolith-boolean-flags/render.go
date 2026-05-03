package cards

import "fmt"

type Card struct {
	Title    string
	Price    float64
	ImageURL string
	Badge    string
	Rating   float64
}

// RenderCard renders a product card HTML snippet.
// Callers use different combinations of flags to control appearance.
func RenderCard(
	c Card,
	isCompact bool,
	isFeatured bool,
	isPromo bool,
	hasFooter bool,
	variant string, // "a", "b", or "c"
) string {
	css := "card"
	if isCompact {
		css += " card--compact"
	}
	if isFeatured {
		css += " card--featured"
	}
	if isPromo {
		css += " card--promo"
	}

	img := ""
	if !isCompact {
		img = fmt.Sprintf(`<img src="%s" class="card__image">`, c.ImageURL)
	}

	badge := ""
	if isPromo && c.Badge != "" {
		badge = fmt.Sprintf(`<span class="badge badge--promo">%s</span>`, c.Badge)
	} else if isFeatured && c.Badge != "" {
		badge = fmt.Sprintf(`<span class="badge badge--featured">%s</span>`, c.Badge)
	}

	rating := ""
	if variant == "b" || variant == "c" {
		rating = fmt.Sprintf(`<div class="card__rating">%.1f</div>`, c.Rating)
	}

	title := c.Title
	if variant == "a" {
		title = fmt.Sprintf(`<h3 class="card__title">%s</h3>`, c.Title)
	} else {
		title = fmt.Sprintf(`<h4 class="card__title card__title--sm">%s</h4>`, c.Title)
	}

	price := fmt.Sprintf(`<span class="card__price">$%.2f</span>`, c.Price)
	if isPromo {
		price = fmt.Sprintf(`<span class="card__price card__price--sale">$%.2f</span>`, c.Price)
	}

	footer := ""
	if hasFooter {
		footer = `<div class="card__footer"><button>Add to cart</button></div>`
		if isFeatured {
			footer = `<div class="card__footer card__footer--featured"><button class="btn--primary">Add to cart</button></div>`
		}
	}

	return fmt.Sprintf(
		`<div class="%s">%s%s%s%s%s%s</div>`,
		css, img, badge, title, price, rating, footer,
	)
}
