package main

import (
	"fmt"
	"reflect"
)

type person struct {
	Name string `json:"NAME"`
	Age  int    `json:"AGE"`
}

func main() {
	rt := reflect.TypeOf(person{})
	field, ok := rt.FieldByName("Name")
	if !ok {
		panic("hmm")
	}

	fmt.Println(field.Tag.Lookup("json"))
}
