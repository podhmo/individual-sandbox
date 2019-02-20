package main

import (
	"container/ring"
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
	g, _ := errgroup.WithContext(context.Background())

	ch := make(chan int)
	// provider
	g.Go(func() error {
		defer close(ch)
		for i := 0; i < 100; i++ {
			fmt.Println("00", i)
			ch <- i
		}
		return nil
	})

	headNch := make(chan []int)
	largestNch := make(chan []int)

	// mediator
	g.Go(func() error {
		defer close(headNch)
		defer close(largestNch)

		var headNItems []int
		var largestNItems []int
		n := 0
		r := ring.New(10)
		for i := range ch {
			fmt.Println("11", i)
			if n < 10 {
				headNItems = append(headNItems, i)
			}
			n++
			r.Value = i
			r = r.Next()
		}
		headNch <- headNItems

		for i := 0; i < r.Len(); i++ {
			largestNItems = append(largestNItems, r.Value.(int))
			r = r.Next()
		}
		largestNch <- largestNItems
		return nil
	})

	// consumer
	g.Go(func() error {
		for items := range headNch {
			fmt.Println("head", items)
		}
		return nil
	})
	g.Go(func() error {
		for items := range largestNch {
			fmt.Println("largest", items)
		}
		return nil
	})
	return g.Wait()
}
