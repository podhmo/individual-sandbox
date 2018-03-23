package main

import "fmt"

// Foo :
type Foo string

// Foo :
const (
	FooFoo = Foo("foo")
	FooBar = Foo("bar")
	FooBoo = Foo("boo")
)

// String :
func (x Foo) String() string {
	switch x {
	case FooFoo:
		return "foo"
	case FooBar:
		return "bar"
	case FooBoo:
		return "boo"
	default:
		return fmt.Sprintf("Foo(%q)", string(x))
	}
}

func main() {
	fmt.Println(Foo("z"))
	fmt.Println(FooFoo)
}
