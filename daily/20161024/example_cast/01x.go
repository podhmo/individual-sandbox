package main

import "fmt"


func main() {
	type Y int
	type Z Y
	_x := 1
	x := &_x // *int
	fmt.Printf("%T => %T\n", x, Z(*x))
}
