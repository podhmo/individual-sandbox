package main

import (
	"fmt"
	"reflect"
)

type Fooable interface {
	Foo() string
}

type A struct {
	Name string
}
type B struct {
	A
}

func (a A) Foo() string {
	return fmt.Sprintf("%p - a.%s", &a, a.Name)
}

func (b *B) Foo() string {
	return fmt.Sprintf("%p - &b.%s", &b, b.Name)
}

func foo(o interface{}) {
	v := reflect.ValueOf(o)
	f := v.Interface().(Fooable)
	fmt.Println(f.Foo())
}

func main() {
	a := A{Name: "foo"}
	{
		fmt.Printf("original: %p\n", &a)
		foo(a)
		foo(&a)
	}
	fmt.Println("----------------------------------------")
	{
		b := B{A: a}
		fmt.Printf("original: %p\n", &b)
		// foo(b) // panic
		foo(&b)
	}
}
