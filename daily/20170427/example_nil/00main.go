package main

import "fmt"

type S struct {
	Name string
}

func f(s *S) *S {
	if s == nil {
		return nil
	}
	return s
}

func main() {
	var s S
	fmt.Println(f(&s))
	fmt.Println(f(&s) == nil)
}
