package main

import (
	"fmt"
	"m/findcaller"
	"os"
)

func main() {
	foo()
}

func foo() {
	bar()
}

type person struct {
	Name string
}

func bar() {
	recoverer := findcaller.Recoverer()

	defer func() {
		fmt.Fprintln(os.Stderr, "........................................")
		recoverer()
		fmt.Fprintln(os.Stderr, "........................................")
	}()
	var p *person
	fmt.Println(p.Name) // nil panic
}
