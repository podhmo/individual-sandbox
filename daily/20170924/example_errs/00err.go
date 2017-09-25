package main

import (
	"fmt"

	"github.com/pkg/errors"
)

func f() error {
	return fmt.Errorf("hai")
}

func g() error {
	err := f()
	if err != nil {
		return errors.Wrap(err, "on g")
	}
	return nil
}

func h() error {
	err := g()
	if err != nil {
		return errors.WithMessage(err, "on h")
	}
	return nil
}

func main() {
	err := h()
	if err != nil {
		fmt.Printf("%+v\n", err)
	}
}
