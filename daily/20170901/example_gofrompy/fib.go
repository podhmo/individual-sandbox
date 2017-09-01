package main

import "C"

//export fib
func fib(n uint) uint {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {}
