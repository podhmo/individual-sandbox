package main

import (
	"fmt"
	"log"
)

func mainInner() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fmt.Println("hello")
	return nil
}
func main() {
	fmt.Println("start")
	defer fmt.Println("end")
	mainInner()
}
