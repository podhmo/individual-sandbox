package main

import (
	"fmt"
	"log"
)

func main() {
	log.Printf("!  %v", f())
	log.Printf("!! %+v", f())
}
func f() error {
	return fmt.Errorf("on f: %w", g())
}
func g() error {
	return fmt.Errorf("on g: %w", h())
}
func h() error {
	return fmt.Errorf("hmm")
}
