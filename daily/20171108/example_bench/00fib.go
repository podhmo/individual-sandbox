package main

import "fmt"

func fib(n uint64) uint64 {
	if n < 2 {
		return n
	}
	return fib(n-2) + fib(n-1)
}

func main() {
	fmt.Println(fib(37))
	fmt.Println(fib(37))
	fmt.Println(fib(37))
	fmt.Println(fib(37))
}
