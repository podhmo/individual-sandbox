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
		rt := reflect.TypeOf(v) // or reflect.ValueOf(v).Type()
		fmt.Println(rt.MethodByName("String"))
		// {  <nil> <invalid Value> 0} false
	}
	{
		v := &Foo{}
		rt := reflect.TypeOf(v) // or reflect.ValueOf(v).Type()
		fmt.Println(rt.MethodByName("String"))
		// {String  func(*main.Foo) string <func(*main.Foo) string Value> 0} true
	}
}
