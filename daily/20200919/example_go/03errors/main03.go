package main

import (
	"fmt"
	"log"

	"golang.org/x/xerrors"
)

func main() {
	log.Printf("! %v", f())
	log.Printf("!!%+v", f())
}
func f() error {
	return xerrors.Errorf("on f: %w", g())
}
func g() error {
	return xerrors.Errorf("on g: %w", h())

}
func h() error {
	return fmt.Errorf("hmm")
}
