package main

import (
	"fmt"
	"log"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
)

func main() {
	log.Printf("! %v", f())
	log.Printf("!!%+v", f())
	pp.Println(f())
}
func f() error {
	return fmt.Errorf("on f: %w", g())
}
func g() error {
	return errors.Wrap(h(), "on g")
}
func h() error {
	return fmt.Errorf("hmm")
}
