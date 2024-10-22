package main

import "fmt"

type F[T ~string] struct {
	Value T
}

type Ordering string

type S struct {
	Ordering F[Ordering]
}

func main() {
	// s := S{Ordering: F{"desc"}}
	s := S{Ordering: F[Ordering]{"desc"}}
	fmt.Printf("%#+v\n", s)
}
