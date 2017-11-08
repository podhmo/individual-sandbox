package main

import (
	"fmt"
	"sync"
)

func fib(n uint64) uint64 {
	if n < 2 {
		return n
	}
	return fib(n-2) + fib(n-1)
}

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		fmt.Println(fib(37))
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		fmt.Println(fib(37))
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		fmt.Println(fib(37))
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		fmt.Println(fib(37))
		wg.Done()
	}()
	wg.Wait()
}
