package main

import (
	"fmt"
	"reflect"
)

func main() {
	var x *int64
	fmt.Println(reflect.ValueOf(x).Kind() == reflect.Ptr)
	fmt.Println(reflect.ValueOf(x).Type().String())

	v := reflect.New(reflect.ValueOf(10).Type())
	fmt.Println(v)
}
