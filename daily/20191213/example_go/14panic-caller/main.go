package main

import (
	"fmt"
	"m/findcaller2"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "!", foo())
}

func foo() error {
	return bar()
}

type person struct {
	Name string
}

func bar() (err error) {
	recoverer := findcaller2.Recoverer(&err)
	defer recoverer()

	var p *person
	fmt.Println(p.Name) // nil panic
	return
}
