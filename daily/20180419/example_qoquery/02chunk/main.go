package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	xs := []string{"a", "b", "c", "d", "e"}
	fmt.Println(chunk(xs, 2))
	return nil
}

func chunk(xs []string, n int) [][]string {
	var r [][]string

	for i := 0; i < len(xs); i += n {
		end := i + n
		if end > len(xs) {
			end = len(xs)
		}
		r = append(r, xs[i:end])
	}
	return r
}
