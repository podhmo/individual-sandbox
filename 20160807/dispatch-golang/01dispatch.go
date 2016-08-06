package main

import (
	"fmt"
)

type Fooable interface {
	Foo() string
}

type A struct {
	Name string
}
type B struct {
	*A
}
type C struct {
	*A
}

func (a A) Foo() string {
	return fmt.Sprintf("%p - a.%s", &a, a.Name)
}


func (b B) Foo() string {
	return fmt.Sprintf("b=%p, a=%p - b.%s", &b, b.A, b.Name)
}

func (c *C) Foo() string {
	return fmt.Sprintf("c=%p, a=%p - &c.%s", c, c.A, c.Name)
}

func foo(o Fooable) {
	fmt.Println(o.Foo())
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
		b := B{A: &a}
		fmt.Printf("original: b=%p, a=%p\n", &b, &(b.A))
		foo(b)
		foo(&b)
	}
    fmt.Println("----------------------------------------")
	{
		c := C{A: &a}
		fmt.Printf("original: c=%p, a=%p\n", &c, c.A)
		// foo(c) // error
		foo(&c)
	}
}
