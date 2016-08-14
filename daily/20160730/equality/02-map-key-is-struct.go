package main

import (
	"fmt"
)

type Point struct {
	X, Y int
}

func main() {
	m := map[Point]int{}
    m[Point{X: 10, Y: 20}]++
    m[Point{X: 10, Y: 20}]++
    m[Point{X: 10, Y: 10}]++
	fmt.Printf("%v\n", m)
}
