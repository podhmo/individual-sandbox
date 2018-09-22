package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
)

// ErrNotFound :
var ErrNotFound = fmt.Errorf("not found")

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return errors.Wrap(ErrNotFound, "on run")
}
