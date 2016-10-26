package main

import "fmt"

func main() {
	type a struct {
		v int
	}
	a := a{v: 10}
	fmt.Println(a)
}
