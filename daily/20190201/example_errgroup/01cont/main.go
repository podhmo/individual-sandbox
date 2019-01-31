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
	g, _ := errgroup.WithContext(context.Background())

	ch := make(chan []int)
	g.Go(func() error {
		defer close(ch)
		var xs []int
		for i := 0; i < 10; i++ {
			time.Sleep(100 * time.Millisecond)
			fmt.Println(i * i)
			xs = append(xs, i*i)
		}
		ch <- xs
		return nil
	})

	g.Go(func() error {
		for xs := range ch {
			fmt.Println("end", xs)
		}
		return nil
	})

	return g.Wait()
}
