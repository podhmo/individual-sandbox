package main

import "fmt"

func main() {
	xs := []int{1, 2, 3}
	fmt.Println(xs)
	var ys []int
	for j := len(xs) - 1; j >= 0; j-- {
		ys = append(ys, xs[j])
	}
	fmt.Println(ys)
}
