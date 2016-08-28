package main

import (
	"fmt"
)

const (
	N int = 100000
)

func main() {
	c := make(chan int, N)

	go func() {
		for i := 0; i < N; i++ {
			c <- i
		}
        close(c)
	}()
	for n := range c {
		fmt.Println(n)
	}
}
