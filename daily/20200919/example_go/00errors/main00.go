package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
)

func main() {
	log.Printf("! %v", f())
	log.Printf("!!%+v", f())
}
func f() error {
	return errors.WithMessage(g(), "on f")
}
func g() error {
	return errors.Wrap(h(), "on g")
}
func h() error {
	return fmt.Errorf("hmm")
}
