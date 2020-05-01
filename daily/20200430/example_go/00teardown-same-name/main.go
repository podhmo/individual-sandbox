package main

import "fmt"

type X struct{}

func NewX() (*X, func()) {
	fmt.Println("new X")
	return &X{}, func() {
		fmt.Println("bye X")
	}
}

type Y struct{}

func NewY() (*Y, func()) {
	fmt.Println("new Y")
	return &Y{}, func() {
		fmt.Println("bye Y")
	}
}

func main() {
	x, teardown := NewX()
	defer teardown()
	y, teardown := NewY()
	defer teardown()

	fmt.Println(x, y)
}
