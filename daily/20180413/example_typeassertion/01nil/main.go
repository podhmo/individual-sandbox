package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var x *int
	var i interface{}
	i = x

	// panic: runtime error: invalid memory address or nil pointer dereference
	if x, _ := i.(*int); x != nil {
		fmt.Println(*x + 1)
	}
	fmt.Println("end")
	return nil
}
