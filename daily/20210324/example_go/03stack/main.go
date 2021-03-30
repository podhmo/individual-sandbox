package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var s []string
	fmt.Println(s)
	s = append(s, "foo")
	fmt.Println(s)
	s = append(s, "bar")
	fmt.Println(s)
	s = append(s, "boo")
	fmt.Println(s)
	s = s[:len(s)-1] // pop
	fmt.Println(s)
	s = s[:len(s)-1] // pop
	fmt.Println(s)
	s = s[:len(s)-1] // pop
	fmt.Println(s)
	return nil
}
