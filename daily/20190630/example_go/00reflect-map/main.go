package main

import (
	"fmt"
	"reflect"
)

func main() {
	m := map[string]string{"x": "y"}
	var iface interface{}
	iface = m

	rv := reflect.ValueOf(iface)

	fmt.Println(rv.MapIndex(reflect.ValueOf("x")).String())
	fmt.Println(rv.MapIndex(reflect.ValueOf("z")).String())
}
