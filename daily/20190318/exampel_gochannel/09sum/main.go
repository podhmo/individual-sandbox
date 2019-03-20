package main

import (
	"fmt"
)

func sum(xs []int) int {
	if len(xs) == 0 {
		return 0
	}
	return xs[0] + sum(xs[1:])
}

func main() {
	fmt.Println(sum([]int{1, 2, 3, 4, 5}))
    // Output:
    // 15
}
