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
type PrinterResolver interface {
	Printer(context.Context) (Printer, error)
}

type WithContext interface {
	WithContext(ctx context.Context)
}

// ---------- foo --------------------
func Foo(p Printer) {
	p.Println("Hello Foo")
}

type FooHandler struct {
	Resolver interface{ PrinterResolver }
}

func (h *FooHandler) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	p, err := h.Resolver.Printer(ctx)
	if err != nil {
		panic(err)
	}
	Foo(p)
}

// ---------- Bar --------------------
var Bar = func(p Printer) {
	p.Println("Hello Bar")
}

type BarHandler struct {
	Resolver interface{ PrinterResolver }
	Do       func(Printer)
}

func (h *BarHandler) WithContext(ctx context.Context) {
	fmt.Println("with context...")
	defer fmt.Println("... with context end")
	p, err := h.Resolver.Printer(ctx)
	if err != nil {
		panic(err)
	}
	h.Do(p)
}

type Resolver struct {
}

func (r *Resolver) Printer(ctx context.Context) (Printer, error) {
	return logger, nil
}

func main() {
	ctx := context.Background()
	resolver := &Resolver{}
	{
		Foo(logger)
		fmt.Println("----------------------------------------")
		(&FooHandler{Resolver: resolver}).WithContext(ctx)

	}
	{
		Bar(logger)
		fmt.Println("----------------------------------------")
		(&BarHandler{Resolver: resolver, Do: Bar}).WithContext(ctx)
	}
}
