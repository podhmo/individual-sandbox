package main

import (
	"fmt"
	"log"

	"github.com/spf13/pflag"
)

var (
	// see: https://github.com/spf13/pflag/pull/171/files
	n    = pflag.IntP("", "n", 0, "number of something")
	name = pflag.String("name", "", "name of something")
)

func main() {
	pflag.Parse()
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

func run() error {
	n := *n
	name := *name

	fmt.Println(n, name)
	return nil
}
