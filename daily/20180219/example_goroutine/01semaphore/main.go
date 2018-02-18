package main

import (
	"context"
	"log"
	"strings"
	"time"

	"golang.org/x/sync/errgroup"
	"golang.org/x/sync/semaphore"
)

func main() {
	st := time.Now()

	N := 20
	M := 6
	log.Printf("start ... tasks=%d, semaphore=%d, expected time=%vs\n", N, M, 0.5*float64((N/M)+1))

	sem := semaphore.NewWeighted(int64(M))
	ctx := context.Background()

	g, ctx := errgroup.WithContext(ctx)
	for i := 0; i < N; i++ {
		i := i
		g.Go(func() error {
			sem.Acquire(ctx, 1) // weight=1
			defer sem.Release(1)
			select {
			case <-ctx.Done():
				return ctx.Err()
			default:
			}
			log.Println("task start", strings.Repeat(" ", i), i)
			time.Sleep(500 * time.Millisecond)
			log.Println("task   end", strings.Repeat(" ", i), i)
			return nil
		})
	}
	if err := g.Wait(); err != nil {
		log.Fatal(err)
	}

	log.Println("... end", time.Now().Sub(st).Seconds())
}
