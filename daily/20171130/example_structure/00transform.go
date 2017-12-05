package main

import (
	"fmt"
)

// P :
type P struct {
	X int
	Y int
}

// Q :
type Q struct {
	X int
	Y int
}

func main() {
	p := P{X: 10, Y: 20}
	q := Q(p)
	fmt.Println(q)
}
