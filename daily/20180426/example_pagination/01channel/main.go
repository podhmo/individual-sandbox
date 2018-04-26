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

func scan(get func(page int) ([]int, error)) <-chan []int {
	ch := make(chan []int)
	go func() {
		defer close(ch)
		i := 0
		for {
			vs, err := get(i)
			if err == io.EOF {
				break
			}
			ch <- vs
			i++
		}
	}()
	return ch
}

func main() {
	for xs := range scan(values) {
		fmt.Println(xs)
	}
}
