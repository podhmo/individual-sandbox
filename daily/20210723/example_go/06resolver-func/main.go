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
type PrinterProvider interface {
	Printer(context.Context) (Printer, error)
}

type WithContext interface {
	WithContext(ctx context.Context)
}

type WithContextFunc func(ctx context.Context)

func (f WithContextFunc) WithContext(ctx context.Context) {
	f(ctx)
}

// ---------- foo --------------------
func Foo(p Printer) {
	p.Println("Hello Foo")
}

func NewFooHandler(provider interface{ PrinterProvider }) WithContext {
	return WithContextFunc(func(ctx context.Context) {
		fmt.Println("with context...")
		defer fmt.Println("... with context end")
		p, err := provider.Printer(ctx)
		if err != nil {
			panic(err)
		}
		Foo(p)
	})
}

// ---------- Bar --------------------
var Bar = func(p Printer) {
	p.Println("Hello Bar")
}

func NewBarHandler(provider interface{ PrinterProvider }) WithContext {
	return WithContextFunc(func(ctx context.Context) {
		fmt.Println("with context...")
		defer fmt.Println("... with context end")
		p, err := provider.Printer(ctx)
		if err != nil {
			panic(err)
		}
		Bar(p)
	})
}

type Provider struct {
}

func (p *Provider) Printer(ctx context.Context) (Printer, error) {
	return logger, nil
}

func main() {
	ctx := context.Background()
	provider := &Provider{}
	{
		Foo(logger)
		fmt.Println("----------------------------------------")
		NewFooHandler(provider).WithContext(ctx)
	}
	{
		Bar(logger)
		fmt.Println("----------------------------------------")
		NewBarHandler(provider).WithContext(ctx)
	}
}
