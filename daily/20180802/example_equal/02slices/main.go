package main

import (
	"fmt"
	"reflect"
)

func main() {
	{
		fmt.Println("[]int")
		xs := []int{1}
		ys := []int{1}
		// ng: xs == ys
		fmt.Println("	", xs, ys, reflect.DeepEqual(xs, ys))
	}
	{
		type XS []int
		fmt.Println("[]int as XS")
		var xs XS
		var ys XS
		xs = []int{1}
		ys = []int{1}
		// ng: xs == ys
		fmt.Println("	", xs, ys, reflect.DeepEqual(xs, ys))
	}
	{
		type XS *[]int
		fmt.Println("*[]int as XS")
		var xs XS
		var ys XS
		xs = &[]int{1}
		ys = &[]int{1}
		fmt.Println("	", xs, ys, xs == ys, reflect.DeepEqual(xs, ys))
	}
	{
		type X int
		fmt.Println("[]int as []X")
		xs := []int{1}
		ys := []X{1}
		// ng: xs == ys
		fmt.Println("	", xs, ys, reflect.DeepEqual(xs, ys))
	}
}
