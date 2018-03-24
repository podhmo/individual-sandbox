package main

import (
	"context"
	"errors"
	"fmt"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	ctx := context.Background()
	g, ctx := errgroup.WithContext(ctx)
	g.Go(func() error {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	one")
		return nil
	})
	g.Go(func() error {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	two")
		return errors.New("hmm")
	})
	fmt.Println("wait...")
	if err := g.Wait(); err != nil {
		fmt.Println("err!", err)
	}
	fmt.Println("done")
}
