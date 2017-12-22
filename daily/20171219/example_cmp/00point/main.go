package main

import "fmt"

// Point :
type Point struct {
	X int
	Y int
}

func main() {
	p0 := Point{X: 10, Y: 20}
	p1 := Point{X: 10, Y: 200}
	p2 := Point{X: 100, Y: 20}
	fmt.Println(p0, p1, p0 < p1) // yah
}
