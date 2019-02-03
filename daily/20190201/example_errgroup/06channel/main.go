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
	type R struct {
		N  int
		NN int
	}
	var r R
	ch := make(chan int)

	g, _ := errgroup.WithContext(context.Background())
	for k := 0; k < 1000; k++ {
		g.Go(func() error {
			for i := 0; i < 10; i++ {
				// fmt.Println("n", k, i)
				ch <- 1
				// time.Sleep(100 * time.Millisecond)
			}
			return nil
		})
	}

	done := make(chan struct{})
	go func() {
		for i := range ch {
			r.N += i
		}
		done <- struct{}{}
	}()

	if err := g.Wait(); err != nil {
		return err
	}
	close(ch)
	fmt.Println(r)
	<-done
	fmt.Println("ok")
	return nil
}
