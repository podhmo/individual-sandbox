package main

import (
	"./bar"
	"./foo"
)

func main() {
	foo.Foo{}.M()

	// Foo is not Exported
	// bar.Foo{}.M()

	// Bar hasn't Foo's method
	// bar.Bar{}.M()

	// embedded is not activated?
	bar.Bar2{Foo: foo.Foo{}}.M()
	(bar.Bar2{Foo: foo.Foo{}}.Foo).M()

	// but if foo.Foo.M() method's name is foo.Foo.Foo()
	// this is bad: `bar.Bar2{Foo: foo.Foo{}}.Foo()`
}
