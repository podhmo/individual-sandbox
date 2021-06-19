package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func F(x x) {
	fmt.Println("ok", x)
}

type x string

func run() error {
	F("foo")

	// ng
	F(fmt.Sprintf("ng"))
	return nil
}
