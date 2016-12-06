package main

import (
	"context"
	"log"
	"time"

	"github.com/wacul/batch"
)

func main() {
	c := 0
	loop := &batch.Loop{
		Worker: func(ctx context.Context) {
			c++
			log.Println(c)
			// delayler
			select {
			case <-time.After(intervalFromContext(ctx)):
				return
			case <-ctx.Done():
				return
			}
		},
	}
	loop.Run(intervalContext(context.Background(), 200 * time.Millisecond))
}

// IntervalKey :
type IntervalKey struct{}

var intervalKey = IntervalKey{}

func intervalFromContext(ctx context.Context) time.Duration {
	return ctx.Value(intervalKey).(time.Duration)
}
func intervalContext(ctx context.Context, interval time.Duration) context.Context {
	return context.WithValue(ctx, intervalKey, interval)
}
