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

type WithContext interface {
	WithContext(ctx context.Context)
}

// ---------- foo --------------------
func Foo(p Printer) {
	p.Println("Hello Foo")
}

type FooHandler struct {
	Printer Printer
}

func (handler *FooHandler) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	Foo(handler.Printer)
}

// ---------- Bar --------------------
var Bar = func(p Printer) {
	p.Println("Hello Bar")
}

type BarHandler struct {
	Printer Printer
}

func (handler *BarHandler) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	Bar(handler.Printer)
}

func main() {
	ctx := context.Background()
	{
		Foo(logger)
		fmt.Println("----------------------------------------")
		(&FooHandler{Printer: logger}).WithContext(ctx)

	}
	{
		Bar(logger)
		fmt.Println("----------------------------------------")
		(&BarHandler{Printer: logger}).WithContext(ctx)
	}
}
