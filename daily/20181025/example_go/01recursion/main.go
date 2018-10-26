package main

import "fmt"

func main() {
	// recursion
	{
		var fib func(int) int
		fib = func(x int) int {
			if x <= 0 {
				return 0
			}
			if x == 1 {
				return 1
			}
			return fib(x-2) + fib(x-1)
		}
		fmt.Println(fib(10))
	}

	// recursion with inner function
	{
		var inner func(int, int, int) int
		var fib func(int) int
		fib = func(x int) int {
			inner = func(i int, a, b int) int {
				if x < i {
					return a
				}
				return inner(i+1, b, a+b)
			}
			return inner(1, 0, 1)
		}
		fmt.Println(fib(10))
	}

	// mutual recirsion
	{
		var odd func(int) bool
		var even func(int) bool
		odd = func(x int) bool {
			if x <= 0 {
				return false
			}
			return even(x - 1)
		}
		even = func(x int) bool {
			if x <= 0 {
				return true
			}
			return odd(x - 1)
		}
		fmt.Println(odd(1), odd(2), odd(3))
	}
}
