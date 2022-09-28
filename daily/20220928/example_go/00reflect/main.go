package main

import (
	"fmt"
	"reflect"
)

func Touch[T any](ob *T) {
	rv := reflect.ValueOf(ob).Elem()

	{
		rf := rv.FieldByName("Name") // Field()のほうが高速
		rf.SetString("*bar*")
	}
}

type Person struct {
	Name string
	Age  int
}

func main() {
	ob := &Person{Name: "foo", Age: 20}
	fmt.Printf("before: %#+v\n", ob)
	Touch(ob)
	fmt.Printf("after : %#+v\n", ob)
}
