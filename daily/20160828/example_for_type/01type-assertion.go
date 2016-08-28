package main

import (
	"fmt"
)

type stringer interface {
	String() string
}

type mystring string

func (s mystring) String() string {
	return fmt.Sprintf("%T: %s", s, string(s))
}

func main() {
    var s stringer = mystring("foo")
	target, _ := s.(mystring)
	fmt.Printf("%[1]T, %#+[1]v\n", target)
}
