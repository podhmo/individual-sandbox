package main

import (
	"fmt"
)

type Greeter interface {
	Greet() string
}

type Foo struct {
	name string
}

func (f *Foo) Greet() string {
	return fmt.Sprintf("pointer of %s", f.name)
}

func foo(f Greeter) string {
	return f.Greet()
}

func main() {
	f := Foo{name: "foo"}
	// fmt.Println(foo(f))  // compile error
	fmt.Println(foo(&f)) // => "pointer of foo"
}
