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

func main() {
	ns := ints(1, 7)
	ms := ints(1, 5)
	var wg sync.WaitGroup
	wg.Add(3)

	go func() {
		for n := range ns {
			fmt.Println("n0", n)
		}
		wg.Done()
	}()
	go func() {
		for n := range ns {
			fmt.Println("n1", n)
		}
		wg.Done()
	}()
	go func() {
		for n := range ms {
			fmt.Println("m0", n)
		}
		wg.Done()
	}()

	wg.Wait()
	fmt.Println("ok")
}
