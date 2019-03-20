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
	ch2 := make(chan int)
	wg.Add(3)
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
	go func() {
		defer close(ch2)
		defer wg.Done()
		for _, x := range []int{10, 20, 30} {
			ch1 <- x
			time.Sleep(30 * time.Millisecond)
		}
	}()
	wg.Add(1)
	go func() {
		defer wg.Done()
		var r []int

		var merge func(chs []<-chan int) <-chan int
		merge = func(chs []<-chan int) <-chan int {
			switch len(chs) {
			case 0:
				ch := make(chan int)
				close(ch)
				return ch
			case 1:
				return chs[0]
			default:
				ch := make(chan int)
				go func() {
					defer close(ch)
					restCH := merge(chs[1:])
					for n := 2; n > 0; {
						select {
						case x, ok := <-chs[0]:
							if !ok {
								n--
								chs[0] = nil
								continue
							}
							ch <- x
						case x, ok := <-restCH:
							if !ok {
								n--
								restCH = nil
								continue
							}
							ch <- x
						}
					}
				}()
				return ch
			}
		}
		for x := range merge([]<-chan int{ch0, ch1, ch2}) {
			r = append(r, x)
		}
		fmt.Println(r)
	}()
	wg.Wait()
	// Output:
	// [1 -1 10 2 3 -2 20 4 -3 5 30 -4 -5]
}
