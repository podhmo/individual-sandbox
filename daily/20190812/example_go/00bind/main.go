package main

import (
	"fmt"
	"reflect"
)

type person struct {
	Name string
}

func main() {
	{
		fmt.Println("----------------------------------------")
		var ob interface{}
		fmt.Printf("before: %+v\n", ob)
		bind(&ob, &person{Name: "foo"})
		fmt.Printf("after: %+v\n", ob)
	}

	{
		fmt.Println("----------------------------------------")
		var ob person
		fmt.Printf("before: %+v\n", ob)
		bind2(&ob, &person{Name: "foo"})
		fmt.Printf("after: %+v\n", ob)
	}

	{
		fmt.Println("----------------------------------------")
		var ob person
		fmt.Printf("before: %+v\n", ob)
		bind3(&ob, &person{Name: "foo"})
		fmt.Printf("after: %+v\n", ob)
	}
}

func bind(dst *interface{}, src interface{}) {
	*dst = src
}

func bind2(dst interface{}, src interface{}) {
	if reflect.TypeOf(dst) != reflect.TypeOf(src) {
		fmt.Println("\t dst", reflect.TypeOf(dst).String())
		fmt.Println("\t src", reflect.TypeOf(src).String())
		panic("oops")
	}
	rdst := reflect.ValueOf(dst)
	rsrc := reflect.ValueOf(src)
	rdst.Elem().Set(rsrc.Elem())
}

func bind3(dst *person, src *person) {
	*dst = *src
}
