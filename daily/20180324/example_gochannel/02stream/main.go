package main

import "fmt"

func ints(i, n int) chan int {
	ch := make(chan int)
	go func() {
		for ; i < n; i++ {
			i := i
			ch <- i
		}
		close(ch)
	}()
	return ch
}

func main() {
	ns := ints(1, 5)
	ms := ints(1, 5)

	for n := range ns {
		fmt.Println(n)
	}
	for n := range ms {
		fmt.Println(n)
	}
}
