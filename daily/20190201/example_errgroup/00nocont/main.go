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

	g.Go(func() error {
		var xs []int
		for i := 0; i < 10; i++ {
			time.Sleep(100 * time.Millisecond)
			fmt.Println(i * i)
			xs = append(xs, i*i)
		}
		fmt.Println(xs) // cont
		return nil
	})

	g.Go(func() error {
		var xs []int
		for i := 0; i < 10; i++ {
			time.Sleep(100 * time.Millisecond)
			fmt.Println(i * i)
			xs = append(xs, i*i)
		}
		fmt.Println(xs) // cont
		return nil
	})

	return g.Wait()
}
