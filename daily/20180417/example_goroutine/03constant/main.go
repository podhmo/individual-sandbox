package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"
)

type ref struct {
	i int
	c int
}

func once(ctx context.Context, fn func() *ref) <-chan *ref {
	ch := make(chan *ref)
	go func() {
		ref := fn()
		for {
			select {
			case ch <- ref:
			case <-ctx.Done():
				fmt.Println("break", ref.i, ref.c)
				close(ch)
				return
			}
		}
	}()
	return ch
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Millisecond)
	_ = cancel
	qs := make([]<-chan *ref, 10)
	for i := 0; i < 10; i++ {
		i := i
		qs[i] = once(ctx, func() *ref {
			fmt.Println("calc", i)
			return &ref{i: i, c: 0}
		})
	}

	var wg sync.WaitGroup
	for _, q := range qs {
		q := q
		wg.Add(1)
		go func() {
			for ref := range q {
				fmt.Println(ref.i, ref.c)
				ref.c++
			}
			wg.Done()
		}()
	}
	wg.Wait()
	return nil
}
