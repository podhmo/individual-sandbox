package main

import (
	"fmt"
)

func main() {
	// undefined: fib
	fib := func(n int) int {
		if n <= 1 {
			return n
		}
		return fib(n-1) + fib(n-2)
	}
	fmt.Println(fib(20))
}
