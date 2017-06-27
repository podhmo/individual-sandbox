package main

import "github.com/k0kubun/pp"

// T :
type T int

//
const (
	TX = T(0)
	TY = T(1)
)

// A :
type A struct {
	T T
	X X
	Y Y
}

// X :
type X struct {
}

// Y :
type Y struct {
}

func main() {
	var a A
	pp.Println(a)
}
