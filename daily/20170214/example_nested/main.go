package main

import "github.com/davecgh/go-spew/spew"

// B :
type B struct {
	i int
	j int
	k int
}

// A :
type A struct {
	b B
}

func main() {
	a := A{}
	spew.Dump(a)
	b := B{i: 10}
	a.b = b
	spew.Dump(a)
}
