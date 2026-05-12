package pricing

import "fmt"

// ApplyDiscount returns price reduced by the given percent.
//
// percent must be in the closed interval [0, 100]. Values outside that range
// are rejected with an error rather than clamped, because callers have already
// validated the input upstream and a silent clamp would hide a bug.
//
// price must be non-negative; negative prices represent a refund flow that
// goes through a different code path.
func ApplyDiscount(price, percent float64) (float64, error) {
	if price < 0 {
		return 0, fmt.Errorf("price must be non-negative, got %v", price)
	}
	if percent < 0 || percent > 100 {
		return 0, fmt.Errorf("percent must be in [0, 100], got %v", percent)
	}
	return price * (1 - percent/100), nil
}
