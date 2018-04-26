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

func scan(get func(page int) ([]int, error), fn func([]int) (next bool)) error {
	i := 0
	for {
		vs, err := get(i)
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		if next := fn(vs); !next {
			return nil
		}
		i++
	}
	return nil
}

func main() {
	if err := scan(values, func(xs []int) bool {
		fmt.Println(xs)
		return true
	}); err != nil {
		log.Fatal(err)
	}
}
