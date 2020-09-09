package main

import (
	"log"
	"github.com/pkg/errors"
	"errors"
)


func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	return f()
}

func f() error {
	return errors.Wrap(g(), "on f")
}

func g() error {
	return errors.New("xxx")
}
