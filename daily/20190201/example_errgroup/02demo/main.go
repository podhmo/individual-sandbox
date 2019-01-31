package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	g, ctx := errgroup.WithContext(ctx)

	save := func(id string, xs []int) error {
		fmt.Println("**SAVE**", id, xs)
		return nil
	}

	run := func(id string, n int) error {
		var r []int
		for i := 0; i < n; i++ {
			fmt.Println("run", id, i)
			time.Sleep(100 * time.Millisecond)
			r = append(r, i*i)
		}
		return save(id, r)
	}

	for i := 0; i < 2; i++ {
		{
			id := fmt.Sprintf("A%d", i)
			g.Go(func() error {
				return run(id, 5)
			})
		}
		{
			id := fmt.Sprintf("B%d", i)
			g.Go(func() error {
				return run(id, 5)
			})
		}

	}
	return g.Wait()
}
