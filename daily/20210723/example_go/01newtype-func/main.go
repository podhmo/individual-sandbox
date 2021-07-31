package main

import (
	"context"
	"fmt"
)

type WithContext interface {
	WithContext(ctx context.Context)
}

// ---------- foo --------------------
func Foo() {
	fmt.Println("Hello Foo")
}

type FooAction func()

func (action FooAction) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	action()
}

// ---------- Bar --------------------
type BarAction func()

var Bar BarAction = func() {
	fmt.Println("Hello Bar")
}

func (action BarAction) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	action()
}

var (
	MyFoo FooAction = Foo
	MyBar BarAction = Bar
)

func main() {
	{
		MyFoo()
		fmt.Println("----------------------------------------")
		MyFoo.WithContext(context.Background())

	}
	{
		MyBar()
		fmt.Println("----------------------------------------")
		MyBar.WithContext(context.Background())

	}
}
