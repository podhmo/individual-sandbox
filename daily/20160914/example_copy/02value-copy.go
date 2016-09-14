package main

// nested

import (
	"fmt"
)

type Point struct {
	x, y int
}

type Path struct {
	start, end Point
}

func main() {
	a := Path{start: Point{x: 10, y: 20}, end: Point{x: 100, y: 20}}
	fmt.Printf("a -- %[1]T: %#[1]v\n", a)
	b := a
	a.start.x = 100
	a.end = Point{x: 00, y: 100}
	fmt.Printf("a -- %[1]T: %#[1]v\n", a)
	fmt.Printf("b -- %[1]T: %#[1]v\n", b)
}

// a -- main.Path: main.Path{start:main.Point{x:10, y:20}, end:main.Point{x:100, y:20}}
// a -- main.Path: main.Path{start:main.Point{x:100, y:20}, end:main.Point{x:0, y:100}}
// b -- main.Path: main.Path{start:main.Point{x:10, y:20}, end:main.Point{x:100, y:20}}
