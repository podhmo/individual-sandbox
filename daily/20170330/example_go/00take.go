package main

import "fmt"

func take10(nums []int) []string {
	r := make([]string, 0, 10)
	for i, n := range nums {
		if i >= 10 {
			break
		}
		r = append(r, fmt.Sprintf("%d", n))
	}
	return r
}

func main() {
	{
		fmt.Println(take10([]int{1, 2, 3}))
	}
	{
		fmt.Println(take10([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}))
	}
}
