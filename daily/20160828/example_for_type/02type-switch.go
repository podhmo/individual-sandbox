package main

import (
	"fmt"
)

type stringer interface {
	String() string
}

type mystring string
type another string

func (s mystring) String() string {
	return fmt.Sprintf("%T: %s", s, string(s))
}

func (s another) String() string {
	return fmt.Sprintf("another %T: %s", s, string(s))
}

func d(o interface{}) {
	switch s := o.(type) {
	case mystring:
		fmt.Println("mystring", s)
	case string:
		fmt.Println("string", s)
	case stringer:
		fmt.Println("stringer", s)
	default:
		fmt.Println("default", s)
	}
}

func main() {
	fmt.Println("mystring----------------------------------------")
	d(mystring("foo"))
	fmt.Println("string----------------------------------------")
	d("foo")
	fmt.Println("another----------------------------------------")
	d(another("foo"))
	fmt.Println("default----------------------------------------")
	d(10)
}
