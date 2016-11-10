package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name string
	Age  int
}

func main() {
	rt := reflect.ValueOf(&Person{}).Elem()
	for i := 0; i < rt.NumField(); i++ {
		v := rt.Field(i)
		t := rt.Type().Field(i)
		fmt.Printf("name: %v, value: %v\n", t.Name, v.Interface())
	}
}
