package main

import "sort"

// V :
type V struct {
	Nil bool
	V   int
}

func v(n int) V {
	return V{V: n}
}
func n() V {
	return V{Nil: true}
}

func main() {

	xs := []V{v(10), n(), v(20), v(0)}
	sort.Slice(xs, func(i, j int) bool { return xs[i] <= xs[j] })
}
