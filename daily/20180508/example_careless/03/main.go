package main

import (
	"errors"
	"fmt"
)

func main() {
	// return error?
	fmt.Println(f())
}

func f() error {
	var err error
	defer func() {
		err = errors.New("hmm")
	}()
	return err
}
