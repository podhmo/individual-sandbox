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
	var t1 T1
	copy(t1, t0)
	// reflect.Copy(dst reflect.Value, src reflect.Value)
	// t1 := ((interface{})(t0)).(T1)
	fmt.Println(t0, t1)
}
