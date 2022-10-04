package main

import (
	"fmt"
	"reflect"
)

type S struct{}

//go:noinline
func (s *S) Hello() {
	println("Hello")
}

func main() {
	ob := &S{}
	ob.Hello() // Hello-fm
	println(ob.Hello)
	fmt.Println(reflect.ValueOf(ob.Hello).Pointer())
}
