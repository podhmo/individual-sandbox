// +build ignore

package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fmt.Println("ignored")
	return nil
}
