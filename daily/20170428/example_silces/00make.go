package main

import "fmt"

func main() {
	xs := []int{10, 20, 30}
	ys := make([]int, 10)
	copy(ys, xs)
	fmt.Println(ys)
}
