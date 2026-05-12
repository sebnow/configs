package orders

// applyDiscounts is long because the business rules for stacking promotions
// are genuinely intricate — but it has one job: take a raw subtotal plus a
// list of coupons and return the discounted subtotal. Every branch
// contributes to that single outcome. Splitting on length alone would just
// relocate the switch elsewhere.
func applyDiscounts(subtotal int64, coupons []Coupon) (int64, error) {
	discounted := subtotal
	for _, c := range coupons {
		switch c.Kind {
		case CouponPercentOff:
			discounted = discounted - (discounted*int64(c.PercentBP))/10000
		case CouponFlatOff:
			if c.FlatAmount > discounted {
				discounted = 0
			} else {
				discounted -= c.FlatAmount
			}
		case CouponBOGO:
			if c.UnitsEligible >= 2 {
				discounted -= c.UnitPrice
			}
		case CouponFreeShipping:
			discounted -= c.ShippingAmount
			if discounted < 0 {
				discounted = 0
			}
		case CouponLoyaltyTier:
			tierFactor := loyaltyFactor(c.Tier)
			discounted -= (discounted * tierFactor) / 100
		default:
			// unknown coupon kinds simply do not apply
		}
	}
	if discounted < 0 {
		discounted = 0
	}
	return discounted, nil
}

type Coupon struct {
	Kind           CouponKind
	PercentBP      int
	FlatAmount     int64
	UnitsEligible  int
	UnitPrice      int64
	ShippingAmount int64
	Tier           string
}

type CouponKind int

const (
	CouponPercentOff CouponKind = iota
	CouponFlatOff
	CouponBOGO
	CouponFreeShipping
	CouponLoyaltyTier
)

func loyaltyFactor(tier string) int64 {
	switch tier {
	case "silver":
		return 5
	case "gold":
		return 10
	case "platinum":
		return 15
	}
	return 0
}
