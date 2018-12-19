package main

import (
	"log"
	"time"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	log.Printf("hello")
	time.Sleep(1 * time.Second)
	log.Printf("bye")
	return nil
}
