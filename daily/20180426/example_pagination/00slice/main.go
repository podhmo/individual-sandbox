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

func scan(get func(page int) ([]int, error)) ([][]int, error) {
	var r [][]int
	i := 0
	for {
		vs, err := get(i)
		if err == io.EOF {
			break
		}
		if err != nil {
			return r, err
		}
		i++
		r = append(r, vs)
	}
	return r, nil
}

func main() {
	r, err := scan(values)
	if err != nil {
		log.Fatal(err)
	}
	for _, xs := range r {
		fmt.Println(xs)
	}
}
