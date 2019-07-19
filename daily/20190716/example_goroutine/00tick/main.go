package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	ticker := time.NewTicker(1 * time.Second)
	go func() {
		for {
			select {
			case <-ctx.Done():
				fmt.Println("c")
				break
			case t := <-ticker.C:
				fmt.Println("tick", t)
			}
		}
	}()

	sigCH := make(chan os.Signal, 1)
	signal.Notify(sigCH, syscall.SIGINT, syscall.SIGTERM)

	select {
	case <-ctx.Done():
		fmt.Println(ctx.Err())
	case signum := <-sigCH:
		fmt.Println(signum)
		cancel()
	}
}
