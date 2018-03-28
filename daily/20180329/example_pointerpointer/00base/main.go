package main

import "fmt"

type i interface {
	F() string
	G() string
}

type f struct {
}

func (f *f) F() string {
	return "f F"
}

func (f *f) G() string {
	return "f G"
}

type f2 struct {
	*f
}

func useF(i i) {
	fmt.Println(i.F())
}

func main() {
	useF(&f2{})
}
