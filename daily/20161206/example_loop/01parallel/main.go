package main

import (
	"context"
	"log"
	"math/rand"
	"sync"
	"time"

	"github.com/wacul/batch"
)

func main() {
	c := 0
	var m sync.Mutex
	loop := &batch.Loop{
		Worker: func(ctx context.Context) {
			id := idFromContext(ctx)
			m.Lock()
			c++
			log.Println(id, c)
			m.Unlock()
			interval := intervalFromContext(ctx)
			log.Println(id, interval, "waiting")
			// delayler
			select {
			case <-time.After(interval):
				log.Println(id, interval, "waited")
				return
			case <-ctx.Done():
				return
			}
		},
	}

	id := 0
	var idm sync.Mutex
	parallel := &batch.Parallel{
		Parallels: 10,
		Worker: func(ctx context.Context) {
			idm.Lock()
			id++
			ctx = idContext(ctx, id)
			idm.Unlock()
			ctx = intervalContext(ctx, time.Duration(rand.Intn(1000))*time.Millisecond)
			loop.Run(ctx)
		},
	}
	parallel.Run(nil)
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

// IDKey :
type IDKey struct{}

var idKey = IDKey{}

func idFromContext(ctx context.Context) int {
	return ctx.Value(idKey).(int)
}
func idContext(ctx context.Context, ID int) context.Context {
	return context.WithValue(ctx, idKey, ID)
}
