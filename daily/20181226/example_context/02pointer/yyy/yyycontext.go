package yyy

import "context"

var (
	key = "key"
)

// WithContext :
func WithContext(ctx context.Context) context.Context {
	return context.WithValue(ctx, &key, "yyy")
}

// GetValue :
func GetValue(ctx context.Context) string {
	return ctx.Value(&key).(string)
}
