package main

// pointer

import (
	"fmt"
)

type Point struct {
	X, Y int
}

func main() {
	a := &Point{X: 10, Y: 20}
	fmt.Printf("a -- %[1]T: %#[1]v\n", a)
	b := a
	(*a).X = 100
	fmt.Printf("a -- %[1]T: %#[1]v\n", a)
	fmt.Printf("b -- %[1]T: %#[1]v\n", b)
}

// a -- main.Point: main.Point{X:10, Y:20}
// a -- main.Point: main.Point{X:100, Y:20}
// b -- *main.Point: &main.Point{X:100, Y:20}
