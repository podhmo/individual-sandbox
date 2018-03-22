package main

// foo does not implement IFoo (missing Foo method)

import "fmt"

// IFoo :
type IFoo interface {
	Foo() string
}

type foo struct {
	Foo func() string
}

func use(foo IFoo) {
	fmt.Println(foo.Foo())
}

func main() {
	use(foo{Foo: func() string { return "foo" }})
}
