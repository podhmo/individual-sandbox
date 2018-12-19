package main

import (
	"log"
	"time"
)

func main() {
	log.SetFlags(log.Lmicroseconds)
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	log.Printf("hello")
	time.Sleep(100 * time.Millisecond)
	log.Printf("bye")
	return nil
}
