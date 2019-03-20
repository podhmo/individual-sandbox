package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup
	ch := make(chan int)
	wg.Add(1)
	go func() {
		defer close(ch)
		defer wg.Done()
		for _, x := range []int{1, 2, 3, 4, 5} {
			ch <- x
			time.Sleep(10 * time.Millisecond)
		}
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		var r []int
		func() {
			for {
				select {
				case x, ok := <-ch:
					if !ok {
						return
					}
					r = append(r, x)
				}
			}
		}()
		fmt.Println(r)
	}()
	wg.Wait()
	// Output:
	// [1 2 3 4 5]
}
