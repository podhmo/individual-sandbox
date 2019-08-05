package main

import (
	"fmt"
	"os"
)

func main() {
	phrase := "Hi there!"
	e, err := InitializeEvent(phrase)
	if err != nil {
		fmt.Printf("failed to create event: %s\n", err)
		os.Exit(2)
	}
	e.Start()
}
