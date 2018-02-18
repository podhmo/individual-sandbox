package main

import (
	"context"
	"log"
	"strings"
	"time"

	"golang.org/x/sync/errgroup"
	"golang.org/x/time/rate"
)

func main() {
	st := time.Now()

	N := 20
	M := 6 // task/sec
	log.Printf("start ... tasks=%d, semaphore=%d, expected time=%vs\n", N, M, 0.5*float64((N/M)+1))

	limiter := rate.NewLimiter(rate.Every(time.Second/time.Duration(M)), 10)
	ctx := context.Background()

	g, ctx := errgroup.WithContext(ctx)
	for i := 0; i < N; i++ {
		i := i
		g.Go(func() error {
			if err := limiter.Wait(ctx); err != nil {
				return err
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
