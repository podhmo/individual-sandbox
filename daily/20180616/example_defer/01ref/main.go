package main

import (
	"fmt"
)

// F :
type F struct{ Name string }

// Close :
func (f *F) Close() {
	fmt.Println("close", f.Name)
}

// NewF :
func NewF(name string) *F {
	fmt.Println("open", name)
	return &F{Name: name}
}

func f() {
	f := NewF("A.txt")
	defer f.Close()
	f = NewF("B.txt")
	defer f.Close()
}

func g() {
	f := NewF("A.txt")
	defer func() {
		f.Close()
	}()
	f = NewF("B.txt")
	defer func() {
		f.Close()
	}()
}

func main() {
	f()
	fmt.Println("----------------------------------------")
	g()
}
