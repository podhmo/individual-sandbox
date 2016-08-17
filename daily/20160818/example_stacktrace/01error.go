package main

import (
	"fmt"
)

func foo() error {
	return fmt.Errorf("hmm")
}

func main() {
	err := foo()
	if err != nil {
		panic(err)
	}
}
