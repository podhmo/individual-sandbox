package main

import "fmt"

func main() {
	xs := []int{1, 2, 3}
	ys := append(make([]int, 0, len(xs)), xs...)
	xs = append(xs, 10)
	fmt.Println(xs, ys)
}
