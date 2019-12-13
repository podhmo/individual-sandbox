package main

import (
	"context"
	"fmt"
	"m/findcaller2"
	"os"

	"golang.org/x/sync/errgroup"
)

func main() {
	err := run()
	if err != nil {
		fmt.Fprintln(os.Stderr, "!", err)
	}
}

func run() error {
	g, _ := errgroup.WithContext(context.Background())
	g.Go(func() (err error) {
		recoverer := findcaller2.Recoverer(&err)
		defer recoverer()
		foo()
		return
	})
	return g.Wait()
}

type person struct {
	Name string
}

func foo() {
	bar()
}

func bar() {
	var p *person
	fmt.Println(p.Name) // nil panic
}
