package main

import (
	"fmt"
	"sort"
)

// Int :
func Int(n int) *int {
	return &n
}

func main() {
	type P struct {
		X *int
		Y *int
	}

	xs := []P{
		{X: Int(10), Y: Int(20)},
		{X: Int(100), Y: Int(20)},
		{X: Int(10), Y: Int(200)},
		{X: Int(100), Y: Int(200)},
		{X: nil, Y: Int(20)},
		{X: nil, Y: Int(200)},
		{X: Int(100), Y: nil},
		{X: Int(10), Y: nil},
		{X: nil, Y: nil},
	}
	sort.Slice(xs, func(i, j int) bool {
		if xs[i].X == nil {
			if xs[j].X != nil {
				return true
			}

			if xs[i].Y == nil {
				return true
			}
			if xs[j].Y == nil {
				return false
			}
			return *xs[i].Y <= *xs[j].Y
		}

		if xs[j].X == nil {
			return false
		}
		if *xs[i].X != *xs[j].X {
			return *xs[i].X <= *xs[j].X
		}

		if xs[i].Y == nil {
			return true
		}
		if xs[j].Y == nil {
			return false
		}
		return *xs[i].Y <= *xs[j].Y
	})

	for _, x := range xs {
		fmt.Printf("(")
		if x.X == nil {
			fmt.Printf("nil, ")
		} else {
			fmt.Printf("%3d, ", *x.X)
		}
		if x.Y == nil {
			fmt.Printf("nil")
		} else {
			fmt.Printf("%3d", *x.Y)
		}
		fmt.Printf(")\n")
	}
}
