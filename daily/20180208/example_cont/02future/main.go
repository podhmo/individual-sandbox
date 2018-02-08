package main

import "fmt"

func add(n int) func(int) <-chan int {
	fmt.Println("+", n)
	return func(v int) <-chan int {
		fmt.Println("input", v)
		ch := make(chan int)
		go func() {
			fmt.Println("output", v+n)
			ch <- v + n
		}()
		return ch
	}
}

func main() {
	add10 := add(10)
	add100 := add(100)

	i := 1
	ch := add10(i)
	ch2 := add100(<-ch)
	fmt.Println("**fin**", <-ch2)
}
