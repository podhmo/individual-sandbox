package main

import (
	"context"
	"log"
	"math/rand"
)

type coro struct {
	id    int
	start int
	end   int
	q     chan struct{}
}

func (c *coro) Start() (<-chan int, func()) {
	ch := make(chan int)
	go func() {
		for i := c.start; i < c.end; i++ {
			select {
			case <-c.q:
				return
			default:
				log.Println("\t", i, "<-", c.id)
				ch <- i
			}
		}
		log.Println("\t", "finish", c.id)
		close(ch)
	}()
	return ch, func() {
		go func() {
			c.q <- struct{}{}
		}()
	}
}

func iterate(ctx context.Context, coros ...*coro) <-chan int {
	ch := make(chan int)

	type P struct {
		cancel func()
		ch     <-chan int
	}

	var ps []*P
	for _, coro := range coros {
		ch, cancel := coro.Start()
		ps = append(ps, &P{ch: ch, cancel: cancel})
	}

	go func() {
		defer close(ch)
		for {
			i := int(float32(len(ps)) * rand.Float32())
			p := ps[i]
			select {
			case v, ok := <-p.ch:
				if !ok {
					p.cancel()
					if len(ps) <= 1 {
						return
					}
					ps = append(ps[:i], ps[i+1:]...)
					continue
				}
				ch <- v
			case <-ctx.Done():
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
	ctx := context.Background()
	for x := range iterate(ctx,
		&coro{id: 0, end: 10},
		&coro{id: 1, end: 10},
		&coro{id: 2, end: 10},
	) {
		log.Println(x)
	}
	return nil
}
