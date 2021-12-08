package main

import (
	"fmt"
	"m/foo"
)

func main() {
	xs := []foo.Namer{
		foo.Foo(),
		foo.Bar(),
	}
	for _, x := range xs {
		fmt.Println(x.Name())
	}
}
