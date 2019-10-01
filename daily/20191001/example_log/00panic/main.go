package main

import (
	"context"
	"fmt"
	"log"

	"golang.org/x/sync/errgroup"
)

func main() {
	log.SetFlags(log.Lmicroseconds | log.Lshortfile)
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	g, _ := errgroup.WithContext(context.Background())
	type q struct {
		x int
	}
	type p struct {
		q *q
	}
	g.Go(func() error {
		fmt.Println(new(p).q.x + new(p).q.x)
		return nil
	})
	return g.Wait()
}
