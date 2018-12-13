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
	var i int
	defer func() {
		fmt.Println("end", i)
	}()

	time.Sleep(2 * time.Second)
	i++

	return nil
}
