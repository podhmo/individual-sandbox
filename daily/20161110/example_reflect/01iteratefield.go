package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name *string
	Age  *int
}

func main() {
	i := 20
	rt := reflect.ValueOf(&Person{Age: &i}).Elem()
	for i := 0; i < rt.NumField(); i++ {
		t := rt.Type().Field(i)
		v := rt.Field(i)
		if v.CanAddr() {
			if v.IsNil() {
				fmt.Printf("skip: name: %v, value: %v\n", t.Name, v.Interface())
				continue
			}
			v = v.Elem()
		}
		fmt.Printf("name: %v, value: %v\n", t.Name, v.Interface())
	}
}
