package main

import "fmt"

type S struct {
	Name string
	Marker
}

type Marker interface {
	Mark()
}

func main() {
	s := S{"foo", nil}
	fmt.Printf("%#+v\n", s)
}
