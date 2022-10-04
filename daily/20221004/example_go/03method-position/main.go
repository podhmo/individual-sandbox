package main

import (
	"fmt"
	"runtime/debug"
)

type S struct{}

func (s *S) Hello() {
	fmt.Println("Hello")
	debug.PrintStack()
	fmt.Println("hmm")
}

func main() {
	ob := &S{}
	ob.Hello()
}
