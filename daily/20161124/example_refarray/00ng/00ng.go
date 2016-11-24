package main

import (
	"fmt"

	"github.com/davecgh/go-spew/spew"
)

func main() {
	xs := []int{1, 2, 3}
	ys := make([]*int, len(xs))
	for i, x := range xs {
		ys[i] = &x
	}
	fmt.Println("xs")
	spew.Dump(xs)
	fmt.Println("ys")
	spew.Dump(ys)
}
