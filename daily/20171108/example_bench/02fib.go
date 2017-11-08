package main

import (
	"fmt"
	"sync/atomic"
)

func fib(n uint64) uint64 {
	if n < 2 {
		return n
	}
	return fib(n-2) + fib(n-1)
}

func main() {
	n := 4
	k := uint64(0)

	ch := make(chan uint64, n)
	for i := 0; i < n; i++ {
		go func(i int) {
			ch <- fib(37)
			if atomic.AddUint64(&k, 1) >= uint64(n) {
				close(ch)
			}
		}(i)
	}

	for x := range ch {
		fmt.Println(x)
	}
}
