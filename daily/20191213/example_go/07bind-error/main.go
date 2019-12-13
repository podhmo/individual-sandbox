package main

import (
	"errors"
	"fmt"
)

func main() {
	fmt.Println(foo())
}
func foo() (retErr error) {
	defer func() {
		bind(&retErr)
	}()
	fmt.Println("foo")
	return
}

func bind(err *error) {
	*err = errors.New("hmm")
}
