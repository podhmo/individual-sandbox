package main

import (
	"fmt"
	"reflect"
)

type Foo struct {
}

func (f *Foo) String() string {
	return "Foo"
}

func main() {
	{
		z := Foo{}
		var iface fmt.Stringer
		rt := reflect.TypeOf(z)
		fmt.Println(rt.Implements(reflect.TypeOf(&iface).Elem()))
		// false
	}
	{
		z := &Foo{}
		var iface fmt.Stringer
		rt := reflect.TypeOf(z)
		fmt.Println(rt.Implements(reflect.TypeOf(&iface).Elem()))
		// true
	}
}
