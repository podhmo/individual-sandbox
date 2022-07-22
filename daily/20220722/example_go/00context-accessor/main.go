package main

import (
	"context"
	"fmt"
	"time"
)

type contextKey string

type ContextAccessor[T any] struct {
	z T
}

func (a *ContextAccessor[T]) key() contextKey {
	return contextKey(fmt.Sprintf("%T", a.z)) // absolutely unique?
}

func (a *ContextAccessor[T]) Get(ctx context.Context) T {
	return ctx.Value(a.key()).(T)
}
func (a *ContextAccessor[T]) Set(ctx context.Context, v T) context.Context {
	return context.WithValue(ctx, a.key(), v)
}

type TraceID string
type StartTime time.Time

var AccessTraceID = &ContextAccessor[TraceID]{}
var AccessStartTime = &ContextAccessor[StartTime]{}

func main() {
	ctx := context.Background()
	ctx = AccessTraceID.Set(ctx, TraceID("XXXXXXXXXXXXXXXXXXXXXXXx"))
	ctx = AccessStartTime.Set(ctx, StartTime(time.Now()))
	use(ctx)
}

func use(ctx context.Context) {
	fmt.Println("got:", AccessTraceID.Get(ctx))
	time.Sleep(200 * time.Millisecond)

	startTime := AccessStartTime.Get(ctx)
	fmt.Println("since:", time.Since(time.Time(startTime)))
}
