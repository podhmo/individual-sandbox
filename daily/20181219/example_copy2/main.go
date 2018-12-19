package main

import "fmt"

func main() {
	xs := make([]int, 10)
	for i := 0; i < 10; i++ {
		xs[i] = i
	}

	{
		type ints []int
		var ys ints
		copy(ys, xs)
		fmt.Println(ys)
	}

	{
		type ints []int
		ys := ints(make([]int, 10))
		copy(ys, xs)
		fmt.Println(ys)
	}

}
