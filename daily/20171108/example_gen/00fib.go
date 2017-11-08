package main

import "fmt"

func fib(n uint64) <-chan uint64 {
	ch := make(chan uint64)
	go func() {
		for i, j := uint64(0), uint64(1); i < n; i, j = j, i+j {
			ch <- i
		}
		close(ch)
	}()
	return ch
}

func main() {
	for i := range fib(1000) {
		fmt.Println(i)
	}
}
