package main

import "fmt"

type a struct {
	v int
}

func main() {
	a := a{v: 10}
	fmt.Println(a)
}
