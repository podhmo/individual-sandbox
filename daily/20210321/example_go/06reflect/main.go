package main

import (
	"fmt"
	"reflect"
)

func main() {
	xs := []int{1, 2, 3}
	rv := reflect.ValueOf(xs)
	fmt.Println(rv)
	fmt.Println(rv.Len())
	for i := 0; i < rv.Len(); i++ {
		fmt.Println(rv.Index(i))
	}
	fmt.Println("----------------------------------------")

	rf := reflect.TypeOf(xs)
	fmt.Println(rf)
	fmt.Println(rf.Elem())
}
