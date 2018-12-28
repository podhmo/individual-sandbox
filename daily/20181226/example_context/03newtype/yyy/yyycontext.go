package yyy

import "context"

type k string

const (
	key k = "key"
)

// WithContext :
func WithContext(ctx context.Context) context.Context {
	return context.WithValue(ctx, key, "yyy")
}

// GetValue :
func GetValue(ctx context.Context) string {
	return ctx.Value(key).(string)
}
