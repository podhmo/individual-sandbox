package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	cond := sync.NewCond(&sync.Mutex{})
	g, _ := errgroup.WithContext(context.Background())
	g.Go(func() error {
		cond.L.Lock()
		fmt.Println("..")
		cond.Wait()
		defer cond.L.Unlock()
		fmt.Println("foo")
		return nil
	})
	g.Go(func() error {
		cond.L.Lock()
		fmt.Println("..")
		cond.Wait()
		defer cond.L.Unlock()
		fmt.Println("boo")
		return nil
	})
	time.Sleep(10 * time.Millisecond)
	fmt.Println("yay")
	cond.Broadcast()
	return g.Wait()
}
