package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fmt.Println("Hello", strings.Join(os.Args, " "))
	return nil
}
