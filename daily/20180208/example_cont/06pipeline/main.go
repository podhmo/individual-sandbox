package main

import "fmt"

func transform(fn func(int) int) func(<-chan int) <-chan int {
	return func(xs <-chan int) <-chan int {
		ch := make(chan int)
		go func() {
			for x := range xs {
				ch <- fn(x)
			}
			close(ch)
		}()
		return ch
	}
}

func consume(fn func(int)) func(<-chan int) <-chan int {
	return func(xs <-chan int) <-chan int {
		ch := make(chan int)
		go func() {
			for x := range xs {
				fn(x)
			}
			close(ch)
		}()
		return ch
	}
}

func provide(xs ...int) <-chan int {
	ch := make(chan int)
	go func() {
		for _, x := range xs {
			ch <- x
		}
		close(ch)
	}()
	return ch
}

func peek(fn func(int) int) func(int) int {
	return func(i int) int {
		fmt.Println("input", i)
		r := fn(i)
		fmt.Println("output", r)
		return r
	}
}

// TODO: concurrent
// TODO: cancel
// TODO: error management

func main() {
	sq := transform(peek(func(i int) int {
		return i * i
	}))

	dump := consume(func(i int) {
		fmt.Println(i)
	})

	fmt.Println("start")
	for x := range dump(sq(sq(sq(provide(10, 20, 30, 40))))) {
		fmt.Println(x)
	}
	fmt.Println("finish")
}
