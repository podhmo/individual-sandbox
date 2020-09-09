package main

import (
	"log"
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
	return errors.New("xxx")
}
