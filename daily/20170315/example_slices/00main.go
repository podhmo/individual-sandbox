package main

import "fmt"

func sq(xs []int) []int {
	ys := make([]int, len(xs))
	for i := range xs {
		ys[i] = xs[i] * xs[i]
	}
	return ys
}

func f(xs ...int) {
	g(sq(xs)...)
}

func g(xs ...int) {
	fmt.Println(xs)
}

func main() {
	f(1, 2, 3, 4, 5)
}
