package main

import (
	"fmt"
	"reflect"
)

type Foo struct {
	Name string
}

func (Foo) String() string {
	return "Foo"
}
func (*Foo) Say() string {
	return "hello"
}

func main() {
	{
		v := Foo{Name: "xxx"}
		rv := reflect.ValueOf(v)
		method := rv.MethodByName("String")
		fmt.Printf("%[1]T	%[1]v\n", method.Call(nil))
		// []reflect.Value	[Foo]
		fmt.Printf("%[1]T	%[1]v\n", method.Call(nil)[0].String())
		// string	Foo
	}

	{
		v := Foo{Name: "xxx"}
		rv := reflect.ValueOf(v)

		// make pointer
		rptrType := reflect.PtrTo(rv.Type())
		rptr := reflect.New(rptrType.Elem())
		rptr.Elem().Set(rv)

		method := rptr.MethodByName("Say")
		fmt.Printf("%[1]T	%[1]v\n", method.Call(nil))
		// []reflect.Value	[hello]
	}
}
