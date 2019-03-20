package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup

	ch0 := make(chan int)
	ch1 := make(chan int)
	wg.Add(2)
	go func() {
		defer close(ch0)
		defer wg.Done()
		for _, x := range []int{1, 2, 3, 4, 5} {
			ch0 <- x
			time.Sleep(10 * time.Millisecond)
		}
	}()
	go func() {
		defer close(ch1)
		defer wg.Done()
		for _, x := range []int{-1, -2, -3, -4, -5} {
			ch1 <- x
			time.Sleep(20 * time.Millisecond)
		}
	}()
	wg.Add(1)
	go func() {
		defer wg.Done()
		var r []int
		func() {
			for {
				select {
				case x, ok := <-ch0:
					if !ok {
						for y := range ch1 {
							r = append(r, y)
						}
						return
					}
					r = append(r, x)
				case x, ok := <-ch1:
					if !ok {
						for y := range ch0 {
							r = append(r, y)
						}
						return
					}
					r = append(r, x)
				}
			}
		}()
		fmt.Println(r)
	}()
	wg.Wait()
}
