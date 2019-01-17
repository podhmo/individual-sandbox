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
	var dst X
	Copy(&src, &dst)
	fmt.Printf("src -> dst; dst=%#v\n", dst)
}

// Copy :
func Copy(src, dst *X) {
	*dst = *src
}
