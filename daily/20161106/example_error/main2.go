package main

import (
	"fmt"

	"github.com/pkg/errors"
)

type causer interface {
	Cause() error
}

type causerError interface {
	error
	causer
}

func UseError(err error) {
	// ここには必ずpkg/errorsのerrorが来て欲しい
	fmt.Printf("%+v\n", err)
}

func f() causerError {
	return errors.New("hmm").(causerError)
}
func main() {
	UseError(f())
}
