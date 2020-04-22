package main

// this packaage is auto generated

import (
	"flag"
	"fmt"
	"log"
	"os"
)

var Option struct {
	Name string // for `-name`
}

func main() {
	cmd := flag.NewFlagSet("hello", flag.ContinueOnError)

	cmd.StringVar(&Option.Name, "name", "", "-")

	if err := cmd.Parse(os.Args[1:]); err != nil {
		if err != flag.ErrHelp {
			cmd.Usage()
		}
		os.Exit(1)
	}
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	fmt.Printf("hello %s\n", Option.Name)
	return nil
}
