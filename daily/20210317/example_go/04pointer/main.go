package main

import (
	"fmt"
	"reflect"
)

func main() {
	{
		v := 10
		rt := reflect.TypeOf(v)
		fmt.Println(rt.Kind())
	}
	{
		v := 10
		v0 := &v
		rt := reflect.TypeOf(v0)
		fmt.Println(rt.Kind())
	}
	{
		v := 10
		v0 := &v
		v1 := &v0
		rt := reflect.TypeOf(v1)
		fmt.Println(rt.Kind())
	}
}
