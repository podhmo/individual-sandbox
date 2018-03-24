package main

import (
	"fmt"
	"sync"
)

func ints(i, n int) chan int {
	ch := make(chan int)
	go func() {
		for ; i < n; i++ {
			i := i
			ch <- i
		}
		close(ch)
	}()
	return ch
}

func mtonpipe(ns, ms <-chan int) <-chan string {
	ch := make(chan string)
	go func() {
		var wg sync.WaitGroup
		wg.Add(3)

		go func() {
			for n := range ns {
				ch <- fmt.Sprintf("n0:%d", n)
			}
			wg.Done()
		}()
		go func() {
			for n := range ns {
				ch <- fmt.Sprintf("n1:%d", n)
			}
			wg.Done()
		}()
		go func() {
			for n := range ms {
				ch <- fmt.Sprintf("m0:%d", n)
			}
			wg.Done()
		}()
		wg.Wait()
		close(ch)
	}()
	return ch
}

func main() {
	ns := ints(1, 7)
	ms := ints(1, 5)

	for x := range mtonpipe(ns, ms) {
		fmt.Println(x)
	}

	fmt.Println("ok")
}
