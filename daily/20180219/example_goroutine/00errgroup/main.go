package main

import (
	"context"
	"log"
	"strings"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	st := time.Now()

	N := 20
	log.Printf("start ... tasks=%d, expected time=%vs\n", N, 0.5)

	ctx := context.Background()

	g, ctx := errgroup.WithContext(ctx)
	for i := 0; i < N; i++ {
		i := i
		g.Go(func() error {
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
