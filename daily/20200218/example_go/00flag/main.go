package main

import (
	"flag"
	"fmt"
	"log"
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

var (
	n    = flag.Int("n", 0, "number of something")
	name = flag.String("name", "", "name of something")
)

func run() error {
	n := *n
	name := *name

	fmt.Println(n, name)
	return nil
}
