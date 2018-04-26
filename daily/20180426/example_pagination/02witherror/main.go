package main

import (
	"fmt"
	"io"
)

var xs = [][]int{
	{1, 2, 3, 4, 5},
	{10, 20, 30, 40, 50},
	{100, 200, 300, 400, 500},
}

// API
func values(page int) ([]int, error) {
	if len(xs) <= page {
		return nil, io.EOF
	}
	return xs[page], nil
}

func scan(errch chan<- error, get func(page int) ([]int, error)) <-chan []int {
	ch := make(chan []int)
	go func() {
		defer close(ch)
		i := 0
		for {
			vs, err := get(i)
			if err == io.EOF {
				break
			}
			if err != nil {
				errch <- err
				break
			}
			ch <- vs
			i++
		}
	}()
	return ch
}

func main() {
	errch := make(chan error)
	ch := scan(errch, values)

loop:
	for {
		select {
		case xs, ok := <-ch:
			if !ok {
				break loop
			}
			fmt.Println(xs)
		case err := <-errch:
			fmt.Println("!!", err)
			break
		}
	}
}
