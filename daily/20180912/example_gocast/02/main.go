package main

import "fmt"

type S0 struct {
	X int
}
type S1 struct {
	X int
}

type T0 struct {
	X S0
}
type T1 struct {
	X S1
}

func main() {
	n := 10
	s0 := S0{X: n}
	t0 := T0{X: s0}
	t1 := T1(t0)
	fmt.Println(t0, t1)
}
