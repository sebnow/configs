package moderation

import "context"

// CorrelationID is a per-request opaque trace identifier carried through the
// request context. It has no domain semantics — there is no parse rule, no
// equivalence beyond string equality, no risk of confusion with a user or
// content identifier. It legitimately stays a raw string.
type ctxKey struct{}

func WithCorrelationID(ctx context.Context, correlationID string) context.Context {
	return context.WithValue(ctx, ctxKey{}, correlationID)
}

func CorrelationFromContext(ctx context.Context) string {
	v, _ := ctx.Value(ctxKey{}).(string)
	return v
}
