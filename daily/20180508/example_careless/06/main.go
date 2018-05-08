package main

import "fmt"

type S struct{}

func main() {
	// nil?
	var s *S
	fmt.Println("nil?", s, s == nil)
	{
		s := f()
		fmt.Println("nil?", s, s == nil)
	}
}

func f() interface{} {
	var s *S
	return s
}
