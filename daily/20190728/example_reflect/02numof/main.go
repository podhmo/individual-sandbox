package main

import (
	"fmt"
	"reflect"
)

type Foo struct{}

func (f *Foo) String() string {
	return "Foo"
}

func main() {
	{
		v := Foo{}
		rt := reflect.TypeOf(v)
		fmt.Println(rt.NumMethod(), rt.Kind(), rt.Name(), rt.String())
		// 0 struct Foo main.Foo
	}
	{
		v := &Foo{} // pointer
		rt := reflect.TypeOf(v)
		fmt.Println(rt.NumMethod(), rt.Kind(), rt.Name(), rt.String())
		// 1 ptr  *main.Foo
	}
	{
		// Valueからpointerを作る
		v := Foo{}
		rv := reflect.ValueOf(v)
		rt := reflect.PtrTo(rv.Type())
		fmt.Println(rt.NumMethod(), rt.Kind(), rt.Name(), rt.String())
		// 1 ptr  *main.Foo
	}
}
