package main

import (
	"log"
)

// P :
type P struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
