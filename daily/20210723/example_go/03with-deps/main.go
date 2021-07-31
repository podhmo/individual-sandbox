package main

import (
	"context"
	"fmt"
	"log"
	"os"
)

var (
	logger = log.New(os.Stdout, "", 0)
)

type Printer interface {
	Println(...interface{})
}

func WithPrinter(ctx context.Context, p Printer) context.Context {
	return context.WithValue(ctx, "p", p)
}
func MustPrinter(ctx context.Context) Printer {
	p, ok := ctx.Value("p").(Printer)
	if !ok {
		panic("Printer is not registered")
	}
	return p
}

type WithContext interface {
	WithContext(ctx context.Context)
}

// ---------- foo --------------------
func Foo(p Printer) {
	p.Println("Hello Foo")
}

type FooAction func(Printer)

func (action FooAction) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	action(MustPrinter(ctx))
}

// ---------- Bar --------------------
type BarAction func(Printer)

var Bar BarAction = func(p Printer) {
	p.Println("Hello Bar")
}

func (action BarAction) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	action(MustPrinter(ctx))
}

var (
	MyFoo FooAction = Foo
	MyBar BarAction = Bar
)

func main() {
	ctx := context.Background()
	ctx = WithPrinter(ctx, logger)
	{
		MyFoo(logger)
		fmt.Println("----------------------------------------")
		MyFoo.WithContext(ctx)

	}
	{
		MyBar(logger)
		fmt.Println("----------------------------------------")
		MyBar.WithContext(ctx)
	}
}
