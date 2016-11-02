package main

import (
	"fmt"

	"github.com/pkg/errors"
)

func f() error {
	return errors.Wrap(g(), "with f")
}
func g() error {
	return errors.Wrap(h(), "with g")
}
func h() error {
	return fmt.Errorf("hmm")
}

func main() {
	err := f()
	if err != nil {
		fmt.Printf("error is occured:\n%+v\n", err)
	}
}
