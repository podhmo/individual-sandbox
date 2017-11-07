package main

import (
	"context"
	"log"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	ctx := context.Background()
	g, ctx := errgroup.WithContext(ctx)
	for i := 0; i < 30; i++ {
		i := i
		g.Go(func() error {
			log.Printf("b %3d\n", i)
			time.Sleep(1 * time.Second)
			log.Printf("e %3d\n", i)
			return nil
		})
	}
	if err := g.Wait(); err != nil {
		log.Fatal(err)
	}
}
