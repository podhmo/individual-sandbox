package main

import (
	"fmt"
	"math/rand"
	_ "os"
	_ "os/signal"
	"sync"
	"time"
)

func calc(id int, n int) {
	st := time.Now()
	fmt.Printf("start(%2d): cost=%d\n", id, n)
	time.Sleep(time.Duration(n) * time.Millisecond)
	fmt.Printf(" end(%2d): cost=%d, time=%s\n", id, n, time.Now().Sub(st))
}

func runTicker(d time.Duration, st time.Time) func() {
	fmt.Println("****tick: start")
	ticker := time.NewTicker(time.Duration(100) * time.Millisecond)

	stop := make(chan interface{})
	go func() {
	loop:
		for {
			select {
			case t := <-ticker.C:
				fmt.Printf("****tick: %s\n", t.Sub(st))
			case <-stop:
				break loop
			}
		}
	}()

	return func() {
		fmt.Println("****tick: stop")
		ticker.Stop()
		stop <- struct{}{}
		close(stop)
	}
}

func main() {
	var wg sync.WaitGroup
	st := time.Now()
	tickerCloseFn := runTicker(time.Duration(100)*time.Millisecond, st)
	// sigC := make(chan os.Signal, 1)
	// signal.Notify(sigC, os.Interrupt)

	for i := 0; i < 20; i++ {
		i := i
		wg.Add(1)
		go func() {
			calc(i, rand.Intn(2000))
			wg.Done()
		}()
	}

	wg.Wait()
	tickerCloseFn()
	fmt.Printf("end with %s\n", time.Now().Sub(st))
}
