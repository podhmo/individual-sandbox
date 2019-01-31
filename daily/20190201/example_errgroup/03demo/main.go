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

	run := func(id string, n int, ch chan<- []int) error {
		defer close(ch)
		var r []int
		for i := 0; i < n; i++ {
			fmt.Println("run", id, i)
			time.Sleep(100 * time.Millisecond)
			r = append(r, i*i)
		}
		ch <- r
		return nil
	}

	for i := 0; i < 2; i++ {
		{
			id := fmt.Sprintf("A%d", i)
			ch := make(chan []int)

			g.Go(func() error {
				return run(id, 5, ch)
			})
			g.Go(func() error {
				for r := range ch {
					if err := save(id, r); err != nil {
						return err
					}
				}
				return nil
			})
		}
		{
			id := fmt.Sprintf("B%d", i)
			ch := make(chan []int)

			g.Go(func() error {
				return run(id, 5, ch)
			})
			g.Go(func() error {
				for r := range ch {
					if err := save(id, append(r, r...)); err != nil {
						return err
					}
				}
				return nil
			})
		}
	}
	return g.Wait()
}
