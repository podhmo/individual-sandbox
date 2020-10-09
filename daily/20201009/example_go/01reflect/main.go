package main

import (
	"fmt"
	"reflect"
)

type Basic struct {
	X int
	Y float32
}
type Basic2 struct {
	x int
	y float32
}

func main() {
	{
		v1 := reflect.ValueOf(Basic{1, 0.5})
		for i, n := 0, v1.NumField(); i < n; i++ {
			f1 := v1.Field(i)
			fmt.Println(f1, f1.CanSet())
		}
	}

	fmt.Println("----------------------------------------")

	{
		v1 := reflect.ValueOf(Basic2{1, 0.5})
		for i, n := 0, v1.NumField(); i < n; i++ {
			f1 := v1.Field(i)
			fmt.Println(f1, f1.CanSet())
		}
	}
}
