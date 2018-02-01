package main

import "github.com/k0kubun/pp"

func main() {
	xs := []int{5, 4, 3, 2, 1}
	ys := make([]int, 0, len(xs))
	for i, x := range xs {
		if i == 0 {
			ys = append(ys, x)
		} else {
			ys, ys[0] = append(ys[:1], ys[0:]...), x
		}
	}
	pp.Println(xs)
	pp.Println(ys)
}
