package main

import (
	"log"
	"runtime/debug"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	debug.PrintStack()
	return nil
}
