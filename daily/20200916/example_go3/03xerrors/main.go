package main

import (
	"fmt"
	"log"

	"golang.org/x/xerrors"
)

func main() {
	log.Printf("!! %+v\n", f())
	log.Printf("! %v\n", f())
	log.Printf("! %s\n", f())
}

func f() error {
	err := g()
	return xerrors.Errorf("f %w", err)
}
func g() error {
	err := h()
	return xerrors.Errorf("g %w", err)
}
func h() error {
	return fmt.Errorf("h")
}
