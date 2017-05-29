package main

import "fmt"

func add0(x, y int) int {
	return x + y
}

func add1(xs ...int) int {
	r := 0
	for _, x := range xs {
		r += x
	}
	return r
}

func main() {
	vs := []int{10, 20}
	// fmt.Println(add0(vs...)) // compile error
	fmt.Println(add1(vs...))
}
