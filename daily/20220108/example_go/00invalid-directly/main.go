package main

import (
	"fmt"
	"m/parse"
)

func main() {
	p := parse.Person{}
	fmt.Println(p)
	p.Name = "foo" // panic
}
