package main

import (
	"log"
	"fmt"
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
	return fmt.Errorf("on f: %w", g())
}

func g() error {
	return errors.New("xxx")
}
