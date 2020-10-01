package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name string
	Age  int
}

func show(prefix string, rt interface{}, rv interface{}) {
	fmt.Println("@", prefix, rt, rv)
	fmt.Printf("%T, %T\n", rt, rv)
}

func main() {
	// struct
	{
		p := Person{Name: "foo"}
		rt := reflect.TypeOf(p)
		rv := reflect.ValueOf(p)
		show("struct", rt, rv)

		{

			rt := rt.Field(0).Type
			rv := rv.Field(0)
			show("struct.field", rt, rv)

		}

		// pointer
		{
			rt := reflect.TypeOf(&p)
			rv := reflect.ValueOf(&p)
			show("&struct", rt, rv)

			{
				rt := reflect.TypeOf(&p).Elem()
				rv := reflect.ValueOf(&p).Elem()
				show("*&struct", rt, rv)
			}
		}
	}

	// slice
	{
		ob := []int{10}
		rt := reflect.TypeOf(ob)
		rv := reflect.ValueOf(ob)
		show("slice", rt, rv)

		{
			rt := rt.Elem()
			rv := rv.Index(0)
			show("slice[0]", rt, rv)
		}
	}
	// chan
	{
		ob := make(chan uint, 1)
		rt := reflect.TypeOf(ob)
		rv := reflect.ValueOf(ob)
		show("chan", rt, rv)

		{
			rt := rt.Elem()
			rv := rv.Index(0)
			show("chan[0]", rt, rv)
		}
	}
	{
	}
}
