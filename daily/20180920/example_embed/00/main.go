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

func use(i I) {
	i.F()
}

func main() {
	fake := &fake{}
	use(fake)
}
