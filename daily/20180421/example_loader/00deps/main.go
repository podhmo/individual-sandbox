package main

import (
	"log"
	"time"
)

// S :
type S struct{ T time.Time }

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
