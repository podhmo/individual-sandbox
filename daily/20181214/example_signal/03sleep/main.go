package main

import (
	"fmt"
	"log"
	"time"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fmt.Println("sleep", 2)
	time.Sleep(2 * time.Second)
	fmt.Println("end")
	return nil
}
