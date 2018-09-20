package main

import "fmt"

type S0 struct {
	X int
}
type S1 struct {
	X int
}

func use(s S1) {
	fmt.Println(s)
}

func main() {
	s0 := S0{X: 10}
	s1 := S1(s0)
	fmt.Println(s0, s1)
}
