package main

import "fmt"

func IfaceSlice(xs ...interface{}) []interface{} {
	r := make([]interface{}, len(xs))
	for i := range xs {
		r[i] = xs[i]
	}
	return r
}

func main() {
	xs := []int{1, 2, 3, 4, 5}
	ys := IfaceSlice(xs...)
	fmt.Println(ys)
}
