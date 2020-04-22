package main

import (
	"log"
	"os"

	"github.com/integrii/flaggy"
)

func main() {
	parser := flaggy.NewParser("hello")
	parser.Description = "hello message"

	if err := parser.ParseArgs(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}
