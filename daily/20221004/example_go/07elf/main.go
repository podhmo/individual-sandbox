package main

import "runtime"

type S struct{}

//go:noinline
func (s *S) Hello() string { return "Hello" }

func F() {
	ob := &S{}
	message := ob.Hello()
	println(message)
	println(ob.Hello)
	runtime.FuncForPC(0)
}

func main() {
	F()
}
