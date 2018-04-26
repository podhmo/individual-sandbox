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

func main() {
	for i := 0; ; {
		vs, err := values(i)
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println(vs)
		i++
	}
}
