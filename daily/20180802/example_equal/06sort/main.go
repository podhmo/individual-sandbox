package main

import (
	"fmt"
	"sort"

	"github.com/k0kubun/pp"
)

func main() {
	xs := [][]int{
		{1, 2, 3},
		{1, 20, 3},
		{1, 2, 30},
		{10, 2, 3},
		{10, 20, 3},
		{10, 2, 30},
		{10, 20, 30},
	}
	sort.Slice(xs, func(i, j int) bool {
		for k := range xs[i] {
			if xs[i][k] == xs[j][k] {
				continue
			}
			return xs[i][k] <= xs[j][k]
		}
		return true
	})
	for _, x := range xs {
		fmt.Println(x)
	}

	{
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
		indices := make([][]int, len(xs))
		for i, x := range xs {
			var buf []int
			if x.X == nil {
				buf = append(buf, 0)
				buf = append(buf, 0) // dummy
			} else {
				buf = append(buf, 1)
				buf = append(buf, *x.X)
			}
			if x.Y == nil {
				buf = append(buf, 0)
				buf = append(buf, 0) // dummy
			} else {
				buf = append(buf, 1)
				buf = append(buf, *x.Y)
			}
			buf = append(buf, i)
			indices[i] = buf
		}

		sort.Slice(indices, func(i, j int) bool {
			for k := range indices[i] {
				if indices[i][k] == indices[j][k] {
					continue
				}
				return indices[i][k] <= indices[j][k]
			}
			return true
		})

		for _, idx := range indices {
			pp.Println(xs[idx[4]])
		}
	}
}

// Int :
func Int(n int) *int {
	return &n
}
