package main

import "fmt"

func main() {
	// nilが含まれないつもりだった
	type P struct{ N int }

	xs := []int{1, 2, 3, 4, 5}
	ps := make([]*P, len(xs))
	for i, x := range xs {
		if i == 2 {
			continue
		}
		ps[i] = &P{N: x}
	}

	defer func() { fmt.Println(recover()) }()
	for _, p := range ps {
		fmt.Println(p.N)
	}

}
