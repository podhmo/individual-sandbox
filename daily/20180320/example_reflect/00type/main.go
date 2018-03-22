package main

import (
	"fmt"
	"reflect"
)

func main() {
	{
		var x int64
		k := reflect.ValueOf(x).Kind()
		fmt.Println(k == reflect.Int64)
	}
	{
		var x *int64
		k := reflect.ValueOf(x).Kind()
		fmt.Println(k == reflect.Ptr)
	}
}
