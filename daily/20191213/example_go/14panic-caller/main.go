package main

import (
	"fmt"
	"m/findcaller"
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
	defer recoverer()

	var p *person
	fmt.Println(p.Name) // nil panic
}
