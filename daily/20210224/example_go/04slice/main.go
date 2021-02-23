package main

import "fmt"

func main() {
	xs := []int{1, 2, 3}
	ys := xs
	xs[0] = 10
	fmt.Println(xs, ys)
}
