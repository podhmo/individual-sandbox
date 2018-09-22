package main

import (
	"log"

	"github.com/pkg/errors"
)

// ErrNotFound :
var ErrNotFound = errors.New("not found")

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return ErrNotFound
}
