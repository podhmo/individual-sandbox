package main

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/pflag"
)

var (
	// Cmd ...
	Cmd = pflag.CommandLine

	// see: https://github.com/spf13/pflag/pull/171/files
	n    = Cmd.IntP("", "n", 0, "number of something")
	name = Cmd.String("name", "", "name of something")
)

func main() {
	Cmd.Parse(os.Args[1:])
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
