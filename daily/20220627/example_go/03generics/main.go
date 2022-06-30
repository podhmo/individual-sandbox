package main

import "fmt"

type Foo interface {
	Foo()
}
type Bar interface {
	Bar()
}

type impl struct{}

func (i *impl) Foo() { fmt.Println("Foo") }
func (i *impl) Bar() { fmt.Println("Bar") }

func runFoo[T Foo](get func() T) {
	get().Foo()
}
func runBar[T Bar](get func() T) {
	get().Bar()
}

func main() {
	get := func() *impl { return &impl{} }
	// compile error
	runFoo(get)
	runBar(get)
}
