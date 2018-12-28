package xxx

import "context"

const (
	key = "key"
)

// WithContext :
func WithContext(ctx context.Context) context.Context {
	return context.WithValue(ctx, key, "xxx")
}

// GetValue :
func GetValue(ctx context.Context) string {
	return ctx.Value(key).(string)
}
