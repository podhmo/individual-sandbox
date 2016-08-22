package main

import (
	"fmt"
	"golang.org/x/net/context"
	"os"
	"os/signal"
	"sync"
	"time"
)

func task(ctx context.Context, cb func()) {
	defer cb()
	select {
	case <-ctx.Done():
		fmt.Println("canceled")
		return
	default:
		fmt.Println("*task prepare(1s)")
		{
			c0 := time.After(time.Duration(1) * time.Second)
			t := <-c0
			fmt.Printf("*task start(2s) -- %s\n", t)
		}
		{
			c1 := time.After(time.Duration(2) * time.Second)
			t := <-c1
			fmt.Printf("*task end -- %s\n", t)
		}
	}
}

func main() {
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	ctx, cancel = context.WithTimeout(ctx, time.Duration(6)*time.Second)

	sc := make(chan os.Signal, 1)
	signal.Notify(sc, os.Interrupt)

	for i := 0; i < 3; i++ {
		select {
		case s := <-sc:
			if s == os.Interrupt {
				fmt.Println("bye")
				cancel()
			}
		default:
			var wg sync.WaitGroup
			wg.Add(1)
			go task(ctx, func() { wg.Done() })
			wg.Wait()
			fmt.Println("----------------------------------------")
		}
	}
	fmt.Println("ok")
}
