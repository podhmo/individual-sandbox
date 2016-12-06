package main

import (
	"context"
	"log"
	"sync"
	"time"

	"math/rand"

	"github.com/pkg/errors"
	"github.com/wacul/batch"
)

/*
loop {producer -> [consume, consume, consume] -> end}
*/

func attachTick(ctx context.Context) func() {
	tickContext, tickstop := context.WithCancel(ctx)
	ticker := time.NewTicker(100 * time.Millisecond)

	go func() {
	loop:
		for {
			select {
			case t := <-ticker.C:
				log.Println("\t\t\t\ttick ", t)
			case <-tickContext.Done():
				log.Println("\t\t\t\ttick-end")
				break loop
			}
		}
	}()
	return tickstop
}

func provide(n int) <-chan int {
	q := make(chan int, n)
	for i := 0; i < n; i++ {
		q <- i
	}
	close(q)
	return q
}

func main() {
	endWorker := func(ctx context.Context) {
		// tickstop := tickstopFromContext(ctx)
		// tickstop()
		log.Println("\tend")
	}

	consumeWorker := func(ctx context.Context) {
		id := idFromContext(ctx)
		q := qFromContext(ctx)
		ref := errFromContext(ctx)

		for {
			select {
			case <-ctx.Done():
				return
			case i, ok := <-q:
				if !ok {
					return
				}
				cost := time.Duration(rand.Intn(8)*100) * time.Millisecond
				log.Printf("\t\tconsume -- %d: %d %v\n", id, i, cost)
				time.Sleep(cost)
				if rand.Float32() > 0.985 {
					ref.Lock()
					ref.Value = append(ref.Value, errors.Errorf("failed -- %d: %d %v", id, i, cost))
					ref.Unlock()
				}
				log.Printf("\t\tdone    -- %d: %d %v\n", id, i, cost)
			}
		}
	}

	beginWorker := func(ctx context.Context) {
		log.Println("\tbegin")

		// ctx = tickstopContext(ctx, attachTick(ctx))
		ctx = qContext(ctx, provide(50))

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
	}

	loop := &batch.Loop{
		Worker: func(ctx context.Context) {
			ref := &Ref{}
			ctx = errContext(ctx, ref)
			beginWorker(ctx)
			endWorker(ctx)

			var interval time.Duration
			log.Println(ref.Value)
			if len(ref.Value) > 0 {
				interval = intervalFromContext(ctx)
			}

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

// ERRKey :
type ERRKey struct{}

var errKey = ERRKey{}

// Ref :
type Ref struct {
	sync.Mutex
	Value []error
}

func errFromContext(ctx context.Context) *Ref {
	return ctx.Value(errKey).(*Ref)
}
func errContext(ctx context.Context, ref *Ref) context.Context {
	return context.WithValue(ctx, errKey, ref)
}

// QKey :
type QKey struct{}

var qKey = QKey{}

func qFromContext(ctx context.Context) <-chan int {
	return ctx.Value(qKey).(<-chan int)
}
func qContext(ctx context.Context, q <-chan int) context.Context {
	return context.WithValue(ctx, qKey, q)
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
