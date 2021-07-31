package main

// broken

import (
	"context"
	"fmt"
)

type WithContext interface {
	WithContext(context.Context)
}

type Foo interface {
	Foo()
}
type Bar interface {
	Foo()
}

type FooImpl struct{}

func (impl *FooImpl) Foo() {
	fmt.Println("Hello Foo")
}

type BarImpl struct{}

func (impl *BarImpl) Bar() {
	fmt.Println("Hello Bar")
}

func main() {

}
