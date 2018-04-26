package main

import (
	"fmt"
	"io"
	"log"
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

type pair struct {
	vs  []int
	err error
}

func scan(get func(page int) ([]int, error)) <-chan pair {
	ch := make(chan pair)
	go func() {
		defer close(ch)
		i := 0
		for {
			vs, err := get(i)
			if err == io.EOF {
				break
			}
			ch <- pair{vs: vs, err: err}
			i++
		}
	}()
	return ch
}

func main() {
	for pair := range scan(values) {
		if pair.err != nil {
			log.Fatal(pair.err)
		}
		fmt.Println(pair.vs)
	}
}
