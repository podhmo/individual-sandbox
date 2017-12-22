package main

import (
	"fmt"
	"sort"
)

// Point :
type Point struct {
	X int
	Y int
}

func main() {
	points := []Point{{X: 100, Y: 200}, {X: 10, Y: 200}, {X: 100, Y: 20}, {X: 10, Y: 20}}
	fmt.Println(points)
	sort.Slice(points, func(i, j int) bool {
		pi := points[i]
		pj := points[j]
		if pi.X < pj.X {
			return true
		}
		if pi.Y < pj.Y {
			return true
		}
		return false
	})
	fmt.Println(points)
}
