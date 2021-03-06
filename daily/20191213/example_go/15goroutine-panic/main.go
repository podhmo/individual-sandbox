package main

import (
	"fmt"
	"m/findcaller2"
	"os"
	"sync"
)

func main() {
	err := run()
	if err != nil {
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
