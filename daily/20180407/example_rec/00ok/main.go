package main

import (
	"fmt"
)

func main() {
	var fib func(n int) int
	fib = func(n int) int {
		if n <= 1 {
			return n
		}
		return fib(n-1) + fib(n-2)
	}
	fmt.Println(fib(20))
}
