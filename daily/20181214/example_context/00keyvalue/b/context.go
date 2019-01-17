package b

import (
	"context"
)

type b string

const (
	k = b("key")
)

// Set :
func Set(ctx context.Context, value string) context.Context {
	return context.WithValue(ctx, k, value)
}

// Get :
func Get(ctx context.Context) string {
	return ctx.Value(k).(string)
}
