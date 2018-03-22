package main

// foo does not implement IFoo (missing Foo method)

import "fmt"

// IFoo :
type IFoo interface {
	Foo() string
}

type foo struct {
	foo func() string
}

// Foo :
func (f *foo) Foo() string {
	return f.foo()
}

func use(foo IFoo) {
	fmt.Println(foo.Foo())
}

func main() {
	use(&foo{foo: func() string { return "foo" }})
}
