package main

import (
	"fmt"

	"github.com/pkg/errors"
)

var hmm error

func init() {
	hmm = fmt.Errorf("hmm")
}

func f() error {
	return errors.WithMessage(g(), "with f")
}
func g() error {
	return errors.Wrap(h(), "with g")
}
func h() error {
	return hmm
}

func main() {
	err := f()
	if err != nil {
		fmt.Printf("error is occured:\n%+v\n", err)
		fmt.Printf("err = hmm is %v, Cause(err) = hmm is %v\n", err == hmm, errors.Cause(err) == hmm)
	}
}
