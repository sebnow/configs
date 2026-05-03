package quantity

import "fmt"

type Unit int

const (
	Gram Unit = iota
	Kilogram
	Milligram
)

type Quantity struct {
	Amount float64
	Unit   Unit
}

func (q Quantity) ToGrams() float64 {
	switch q.Unit {
	case Kilogram:
		return q.Amount * 1000
	case Milligram:
		return q.Amount / 1000
	default:
		return q.Amount
	}
}

func (q Quantity) Add(other Quantity) Quantity {
	return Quantity{
		Amount: q.ToGrams() + other.ToGrams(),
		Unit:   Gram,
	}
}

func (q Quantity) Scale(factor float64) Quantity {
	return Quantity{
		Amount: q.Amount * factor,
		Unit:   q.Unit,
	}
}

func (q Quantity) String() string {
	switch q.Unit {
	case Kilogram:
		return fmt.Sprintf("%.3f kg", q.Amount)
	case Milligram:
		return fmt.Sprintf("%.3f mg", q.Amount)
	default:
		return fmt.Sprintf("%.3f g", q.Amount)
	}
}
