package main

import (
	"fmt"
	"sort"

	"github.com/k0kubun/pp"
)

// Int :
func Int(n int) *int {
	return &n
}

func main() {
	{
		xs := []*int{nil, Int(10), Int(5), nil, Int(20)}
		sort.Slice(xs, func(i, j int) bool {
			if xs[i] == nil {
				return true
			}
			if xs[j] == nil {
				return false
			}
			return *xs[i] <= *xs[j]
		})
		pp.Println(xs)
	}

	{
		type P struct {
			X int
			Y int
		}
		xs := []P{{X: 10, Y: 20}, {X: 100, Y: 20}, {X: 10, Y: 200}, {X: 100, Y: 200}}
		sort.Slice(xs, func(i, j int) bool {
			if xs[i].X == xs[j].X {
				return xs[i].Y <= xs[j].Y
			}
			return xs[i].X <= xs[j].Y
		})
		fmt.Printf("%+v\n", xs)
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
		pp.Println(xs)
	}
}
