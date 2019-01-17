package main

import "fmt"

// X :
type X struct {
	Name   string
	AgePtr *int
	Label  Label
	Alias  *Label
}

// Label :
type Label struct {
	Name string
}

// T :
type T struct {
	X X
}

func main() {
	i := 10
	src := X{
		Name:   "foo",
		AgePtr: &i,
		Label: Label{
			Name: "FOO",
		},
		Alias: &Label{
			Name: "F",
		},
	}
	var dst T
	dst.X = src
	fmt.Printf("src -> dst; dst=%#v\n", dst)
}
