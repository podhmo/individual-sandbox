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
	start := make(chan struct{})
	g, _ := errgroup.WithContext(context.Background())
	g.Go(func() error {
		fmt.Println("..")
		<-start
		fmt.Println("foo")
		return nil
	})
	g.Go(func() error {
		fmt.Println("..")
		<-start
		fmt.Println("boo")
		return nil
	})
	time.Sleep(10 * time.Millisecond)
	fmt.Println("yay")
	close(start)
	return g.Wait()
}
