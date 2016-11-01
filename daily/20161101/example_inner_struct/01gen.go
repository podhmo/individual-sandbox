package main

import "fmt"

// Greeter :
type Greeter interface {
	Greet() string
}

// SimpleGreeter :
type SimpleGreeter struct {
	greet func() string
}

// Greet :
func (sg *SimpleGreeter) Greet() string {
	return sg.greet()
}

func main() {
	hello := func() string {
		return "hello"
	}
	h := &SimpleGreeter{greet: hello}
	fmt.Println(h.Greet())
}
