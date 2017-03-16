package main

import "fmt"

// P :
type P struct {
	X *int
}

func main() {
	v := 10
	v2 := 100

	p := &P{X: &v}
	p2 := *p
	p2.X = p.X

	p3 := &P{X: &v2}
	*p2.X += *p3.X

	fmt.Println(*p.X)
	fmt.Println(*p2.X)
	fmt.Println(*p3.X)
}
