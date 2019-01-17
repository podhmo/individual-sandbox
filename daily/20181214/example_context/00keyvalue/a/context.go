package a

import (
	"context"
)

type a string

const (
	k = a("key")
)

// Set :
func Set(ctx context.Context, value string) context.Context {
	return context.WithValue(ctx, k, value)
}

// Get :
func Get(ctx context.Context) string {
	return ctx.Value(k).(string)
}
