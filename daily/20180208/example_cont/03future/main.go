package main

import (
	"fmt"
)

func peek(op func(int) <-chan int) func(v int) <-chan int {
	return func(v int) <-chan int {
		fmt.Println("input", v)
		ch := make(chan int)
		go func() {
			r := <-op(v)
			fmt.Println("output", r)
			ch <- r
		}()
		return ch
	}
}

func add(n int) func(int) <-chan int {
	fmt.Println("+", n)
	return func(v int) <-chan int {
		ch := make(chan int)
		go func() {
			ch <- v + n
		}()
		return ch
	}
}

func main() {
	add10 := peek(add(10))
	add100 := peek(add(100))

	i := 1
	ch := add10(i)
	ch2 := add100(<-ch)
	fmt.Println("**fin**", <-ch2)
}
