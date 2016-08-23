package main

import (
	"log"
)

func gen(nums ...int) <-chan int {
	out := make(chan int)
	go func() {
		for _, n := range nums {
			out <- n
		}
		close(out)
	}()
	return out
}

func sq(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		for n := range in {
			out <- n * n
		}
		close(out)
	}()
	return out
}

func main() {
	c := gen(1, 2, 3, 4, 5)
	out := sq(c)

	// consume the output
	for n := range out {
		log.Printf("%d\n", n)
	}
}
