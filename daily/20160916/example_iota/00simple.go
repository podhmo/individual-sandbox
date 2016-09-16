package main

import (
	"log"
)

type status int

const (
	valid status = iota + 1
	invalid
)

func main() {
	log.Println(valid)
	log.Printf("%T: %v\n", valid, valid)
	log.Printf("%T: %v\n", invalid, invalid)
}
