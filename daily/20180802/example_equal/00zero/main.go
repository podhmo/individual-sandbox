package main

import (
	"fmt"
	"reflect"
)

func main() {
	{
		fmt.Println("int")
		xs := []int{}
		ys := []int{}
		fmt.Println(xs, ys, reflect.DeepEqual(xs, ys))
		fmt.Println(xs, ys, reflect.DeepEqual(xs, nil))
		fmt.Println(xs, ys, reflect.DeepEqual(nil, ys))
	}
	{
		fmt.Println("string")
		xs := []string{}
		ys := []string{}
		fmt.Println(xs, ys, reflect.DeepEqual(xs, ys))
		fmt.Println(xs, ys, reflect.DeepEqual(xs, nil))
		fmt.Println(xs, ys, reflect.DeepEqual(nil, ys))
	}
	{
		fmt.Println("S")
		type S struct {
			Name string
		}
		xs := []S{}
		ys := []S{}
		fmt.Println(xs, ys, reflect.DeepEqual(xs, ys))
		fmt.Println(xs, ys, reflect.DeepEqual(xs, nil))
		fmt.Println(xs, ys, reflect.DeepEqual(nil, ys))
	}
	{
		fmt.Println("*S")
		type S struct {
			Name string
		}
		xs := []*S{}
		ys := []*S{}
		fmt.Println(xs, ys, reflect.DeepEqual(xs, ys))
		fmt.Println(xs, ys, reflect.DeepEqual(xs, nil))
		fmt.Println(xs, ys, reflect.DeepEqual(nil, ys))
	}
}
