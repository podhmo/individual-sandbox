package main

import (
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
				log.Println("id", c.id, "val", i)
				ch <- i
			}
		}
		log.Println("id", c.id, "finish")
		close(ch)
	}()
	return ch, func() {
		go func() {
			c.q <- struct{}{}
		}()
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	coros := []*coro{
		&coro{id: 0, end: 10},
		&coro{id: 1, end: 10},
		&coro{id: 2, end: 10},
	}
	var channels []<-chan int
	for _, coro := range coros {
		ch, cancel := coro.Start()
		defer cancel()
		channels = append(channels, ch)
	}

	for i := 0; i < 30; i++ {
		log.Println("use", <-channels[int(3*rand.Float32())])
	}
	return nil
}
