package main

import (
	"context"
	"fmt"
	"reflect"
)

type Todo struct {
	Title string `json:"title"`
}

func ListTodo(ctx context.Context) ([]Todo, error) {
	return []Todo{{Title: "hello"}}, nil
}

// 便利?
// rt.Implements(r Type)
// rt.Assignable(), rt.Comparable(), rt.Convertible()
// rt.Method(), rt.MethodByName(), rt.NumMethod()
// rt.Elem() for Array,Chan,Map,Ptr,Slice
// rt.Field(), rt.FieldByName(), rt.FieldByNameFunc()??
// rt.In(), rt.NumIn(), rt.Out(), rt.NumOut()

func Visit(ob interface{}) {
	// Typeの情報で欲しいのは
	rt := reflect.TypeOf(ob)

	fmt.Println("name is", rt.Name())
	fmt.Println("package path is", rt.PkgPath())
	fmt.Println("string() is ", rt.String())
	fmt.Println("kind is ", rt.Kind())
	fmt.Println("input is")
	for i := 0; i < rt.NumIn(); i++ {
		fmt.Println("	", rt.In(i))
	}
	fmt.Println("output is")
	for i := 0; i < rt.NumOut(); i++ {
		fmt.Println("	", rt.Out(i))
	}
}

func main() {
	Visit(ListTodo)
}
