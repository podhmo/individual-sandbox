package main

import (
	"context"
	"log"
	"sync"
	"time"

	"math/rand"

	"github.com/wacul/batch"
)

/*
loop {begin -> [thick, thick, thick] -> end}
*/

func attachTick(ctx context.Context) func() {
	tickContext, tickstop := context.WithCancel(ctx)
	ticker := time.NewTicker(100 * time.Millisecond)

	go func() {
	loop:
		for {
			select {
			case t := <-ticker.C:
				log.Println("                    tick ", t)
			case <-tickContext.Done():
				log.Println("                    tick-end")
				break loop
			}
		}
	}()
	return tickstop
}

func main() {
	endWorker := func(ctx context.Context) {
		tickstop := tickstopFromContext(ctx)
		tickstop()
		log.Println("end")
	}

	consumeWorker := func(ctx context.Context) {
		cost := time.Duration(rand.Intn(8)*100) * time.Millisecond
		log.Printf("consume -- %d: %v\n", idFromContext(ctx), cost)
		time.Sleep(cost)
		log.Printf("done    -- %d: %v\n", idFromContext(ctx), cost)
	}

	beginWorker := func(ctx context.Context) {
		log.Println("begin")

		ctx = tickstopContext(ctx, attachTick(ctx))

		var m sync.Mutex
		id := 0

		parallel := &batch.Parallel{
			Parallels: 10,
			Worker: func(ctx context.Context) {
				m.Lock()
				id++
				ctx = idContext(ctx, id)
				m.Unlock()
				consumeWorker(ctx)
			},
		}
		parallel.Run(ctx)
		endWorker(ctx)
	}

	loop := &batch.Loop{
		Worker: func(ctx context.Context) {
			interval := intervalFromContext(ctx)

			beginWorker(ctx)

			log.Println(interval, "wait[S]")
			// delayler
			select {
			case <-time.After(interval):
				log.Println(interval, "wait[E]")
				return
			case <-ctx.Done():
				return
			}
		},
	}

	ctx := context.Background()
	ctx = intervalContext(ctx, time.Duration(500)*time.Millisecond)
	loop.Run(ctx)
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

// TickstopKey :
type TickstopKey struct{}

var tickstopKey = TickstopKey{}

func tickstopFromContext(ctx context.Context) func() {
	return ctx.Value(tickstopKey).(func())
}
func tickstopContext(ctx context.Context, tickstop func()) context.Context {
	return context.WithValue(ctx, tickstopKey, tickstop)
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
