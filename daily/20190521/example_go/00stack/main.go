package main

import (
	"runtime/debug"
)

func f() {
	g()
}
func g() {
	h()
}

func h() {
	debug.PrintStack()
}

func main() {
	f()
}
