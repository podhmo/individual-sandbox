package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	return h()
}

func h() error {
	return errors.Wrap(g(), "h")
}
func g() error {
	return errors.Wrap(f(), "g")
}

func f() error {
	return fmt.Errorf("xxx")
}
