package main

import (
	"fmt"
	"log"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	c := loader.Config{}
	c.Import("fmt")
	fmt.Println(c.Load())
	c.Import("encoding/json")
	fmt.Println(c.Load())
	return nil
}
