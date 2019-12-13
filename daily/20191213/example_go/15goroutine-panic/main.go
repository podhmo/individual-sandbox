package main

import (
	"fmt"
	"log"
	"m/findcaller"
	"sync"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		recoverer := findcaller.Recoverer()
		defer recoverer()
		defer wg.Done()
		foo()
	}()
	wg.Wait()
	return nil
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

