package main

import (
	"fmt"

	"github.com/podhmo/individual-sandbox/daily/20230217/example_go/slices"
)

// これが使えない。。

type Tuple2[E0 slices.Ordered, E1 slices.Ordered] struct {
	V0 E0
	V1 E1
}

// Less : t.Less(o) == t < o
func (t Tuple2[E0, E1]) Less(o Tuple2[E0, E1]) bool {
	if t.V0 < o.V0 {
		return true
	}
	if t.V0 > o.V0 {
		return false
	}

	// t.V0 == o.V0
	if t.V1 < o.V1 {
		return true
	}
	return false
}

type Object struct {
	Name string
	X    int
	Y    int
}

func main() {
	t0 := Tuple2[int, int]{10, 20}
	t1 := Tuple2[int, int]{10, 21}
	t2 := Tuple2[int, int]{9, 21}
	fmt.Printf("x < y = %v, x=%v, y=%v\n", t0.Less(t1), t0, t1)
	fmt.Printf("x < y = %v, x=%v, y=%v\n", t0.Less(t2), t0, t2)

	foo := Object{Name: "foo", X: 10, Y: 10}
	bar := Object{Name: "bar", X: 20, Y: 10}
	boo := Object{Name: "boo", X: 20, Y: 20}
	yoo := Object{Name: "bar", X: 10, Y: 20}
	values := []Object{foo, bar, boo, yoo}

	fmt.Println("----------------------------------------")
	fmt.Println(values)
	fmt.Println(slices.SortBy(values, func(v Object) string { return v.Name }))
	fmt.Println(slices.SortBy(values, func(v Object) string { return fmt.Sprintf("%s-%d-%d", v.Name, v.X, v.Y) }))

	// こんな感じのことがしたかった
	// fmt.Println(slices.SortBy(values, func(v Object) Tuple { return Tuple[v.Name, v.X, v.Y]}))
}
