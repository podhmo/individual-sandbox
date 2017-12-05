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

func (q *Q) say() {
	fmt.Printf("q=%#v\n", q)
}

func main() {
	{
		p := P{X: 10, Y: 20}
		q := (*Q)(&p)
		fmt.Printf("p=%#v, q=%#v\n", p, q)
	}
	{
		p := &P{X: 10, Y: 20}
		q := (*Q)(p)
		fmt.Printf("p=%#v, q=%#v\n", p, q)
	}
	{
		p := &P{X: 10, Y: 20}
		q := (Q)(*p)
		fmt.Printf("p=%#v, q=%#v\n", p, q)
	}
}
