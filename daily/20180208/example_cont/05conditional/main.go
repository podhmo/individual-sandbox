package main

import (
	"fmt"
)

func constant(v int) <-chan int {
	ch := make(chan int)
	go func() {
		ch <- v
	}()
	return ch
}

func skipif(p func(int) bool) func(func(v int) <-chan int) func(v int) <-chan int {
	return func(op func(v int) <-chan int) func(v int) <-chan int {
		return branch(p)(constant, op)
	}
}

func branch(p func(int) bool) func(func(v int) <-chan int, func(v int) <-chan int) func(v int) <-chan int {
	return func(tc func(v int) <-chan int, fc func(v int) <-chan int) func(v int) <-chan int {
		return func(v int) <-chan int {
			ch := make(chan int)
			go func() {
				if p(v) {
					v = <-tc(v)
				} else {
					v = <-fc(v)
				}
				ch <- v
			}()
			return ch
		}
	}
}

func trace(begin func(v int), end func(v int)) func(func(v int) <-chan int) func(v int) <-chan int {
	return func(op func(v int) <-chan int) func(v int) <-chan int {
		return func(v int) <-chan int {
			begin(v)
			ch := make(chan int)
			go func() {
				r := <-op(v)
				end(r)
				ch <- r
			}()
			return ch
		}
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
	marked := func(tag string) func(func(int) <-chan int) func(int) <-chan int {
		return trace(func(v int) {
			fmt.Println("input", tag, v)
		}, func(v int) {
			fmt.Println("output", tag, v)
		})
	}

	peek := marked("C")
	add10 := peek(add(10))
	add100 := peek(add(100))
	add1000 := peek(add(1000))

	skipifEven := skipif(func(i int) bool {
		return i%2 == 0
	})

	fmt.Println("----------------------------------------")

	{
		i := 1
		ch := add10(i)
		ch2 := add100(<-ch)
		ch3 := peek(skipifEven(add1000))(<-ch2)
		fmt.Println("**fin**", <-ch3)
	}

	fmt.Println("----------------------------------------")

	{
		i := 0
		ch := add10(i)
		ch2 := add100(<-ch)
		ch3 := peek(skipifEven(add1000))(<-ch2)
		fmt.Println("**fin**", <-ch3)
	}

	{
		tMarked := marked("T")
		fMarked := marked("F")
		op := branch(func(i int) bool {
			return i%2 == 0
		})(tMarked(add100), fMarked(add1000))

		{
			fmt.Println("----------------------------------------")
			i := 0
			fmt.Println("**fin**", <-op(i))
		}
		{
			fmt.Println("----------------------------------------")
			i := 1
			fmt.Println("**fin**", <-op(i))
		}
	}
}
