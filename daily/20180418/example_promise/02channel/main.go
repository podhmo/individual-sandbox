package main

import (
	"context"
	"fmt"
	"time"
)

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	q := make(chan string)
	go func() {
		defer close(q)
		fmt.Println("start")
		select {
		case <-ctx.Done():
			return
		case <-time.After(300 * time.Millisecond):
			fmt.Println("end")
			for {
				select {
				case <-ctx.Done():
					return
				case q <- "ok":
				}
			}
		}
	}()

	fmt.Println("use", <-q)
	fmt.Println("use2", <-q)
	cancel()
}
