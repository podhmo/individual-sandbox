package main

import (
	"context"
	"fmt"
	"log"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx := context.Background()
	g, _ := errgroup.WithContext(ctx)
	for i := 0; i < 10; i++ {
		i := i
		g.Go(func() error {
			fmt.Println(i)
			return nil
		})
	}
	return g.Wait()
}
