package main

import (
	"fmt"
	"os"
	"os/signal"
	"sync"
	"time"
)

func task(cb func()) {
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
		cb()
	}
}

func main() {
	sc := make(chan os.Signal, 1)
	signal.Notify(sc, os.Interrupt)

	for i := 0; i < 2; i++ {
		select {
		case s := <-sc:
			if s == os.Interrupt {
				fmt.Println("bye")
				os.Exit(0)
			}
		default:
			var wg sync.WaitGroup
			wg.Add(1)
			go task(func() { wg.Done() })
			wg.Wait()
			fmt.Println("----------------------------------------")
		}
	}
	fmt.Println("ok")
}
