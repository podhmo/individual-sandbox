package main

import (
	"fmt"
	"log"

	"github.com/integrii/flaggy"
)

var (
	n    = 0
	name = ""
)

func main() {
	flaggy.Int(&n, "n", "", "number of something")
	flaggy.String(&name, "", "name", "name of something")

	flaggy.Parse()
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

func run() error {
	fmt.Println(n, name)
	return nil
}
