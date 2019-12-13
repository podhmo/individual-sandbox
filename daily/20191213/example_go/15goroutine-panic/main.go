package main

import (
	"fmt"
	"m/findcaller2"
	"os"
	"sync"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "!", err)
	}
}

func run() (err error) {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		recoverer := findcaller2.Recoverer(&err)
		defer recoverer()
		defer wg.Done()
		foo()
	}()
	wg.Wait()
	return
	// not return nil
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
