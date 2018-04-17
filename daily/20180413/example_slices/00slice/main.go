package main

import "fmt"

func main() {
	xs := []int{1, 2, 3}
	ys := xs[1:]
	zs := xs[1:]
	ys[0] = 10

	fmt.Println("xs", xs)
	fmt.Println("ys", ys)
	fmt.Println("zs", zs)
}
