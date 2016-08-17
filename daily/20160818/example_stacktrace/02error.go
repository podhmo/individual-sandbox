package main

import (
	"fmt"
	"github.com/pkg/errors"
	"os"
)

func foo() error {
	return errors.Errorf("hmm")
}

func main() {
	err := foo()
	if err != nil {
		fmt.Printf("error: %+v\n", err)
		os.Exit(1)
	}
}
