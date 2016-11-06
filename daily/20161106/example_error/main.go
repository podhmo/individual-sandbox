package main

import (
	"fmt"

	"github.com/pkg/errors"
)

func f() error {
	return fmt.Errorf("hmm")
}

func g() error {
	return f()
}

type causer interface {
	Cause() error
}

type causerError interface {
	error
	causer
}

func UseError(err causerError) {
	// ここには必ずpkg/errorsのerrorが来て欲しい
	fmt.Printf("%+v\n", err)
}

func main() {
	// compile error
	// UseError(g())
	// ここのcastが嫌
	UseError(errors.Wrap(g(), "hmm").(causerError))
}
