package main

import "fmt"

type S0 struct {
	X *int
}
type S1 struct {
	X *int
}

func main() {
	n := 10
	s0 := S0{X: &n}
	s1 := S1(s0)
	fmt.Println(s0, s1)
}
