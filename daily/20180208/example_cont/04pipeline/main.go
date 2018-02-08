package main

import (
	"fmt"
)

func gen(n int) <-chan int {
	ch := make(chan int)
	go func() {
		ch <- n
		close(ch)
	}()
	return ch
}

func sq(xs <-chan int) <-chan int {
	ch := make(chan int)
	go func() {
		for x := range xs {
			ch <- x * x
		}
		close(ch)
	}()
	return ch
}

func dump(xs <-chan int) <-chan int {
	ch := make(chan int)
	go func() {
		for x := range xs {
			fmt.Println(x)
		}
		close(ch)
	}()
	return ch
}

func genN(xs ...int) <-chan int {
	ch := make(chan int)
	go func() {
		for _, x := range xs {
			ch <- x
		}
		close(ch)
	}()
	return ch
}

func main() {
	fmt.Println(<-gen(10))

	fmt.Println("----------------------------------------")
	<-dump(sq(gen(10)))

	fmt.Println("----------------------------------------")
	<-dump(sq(genN(10, 20, 30)))
}
