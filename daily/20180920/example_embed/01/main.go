package main

import (
	"fmt"
)

type I interface {
	F()
	G()
}

type fake struct {
	I
}

func (f *fake) F() {
	fmt.Println("f")
}

func main() {
	fake := &fake{}
	fake.G()
}
