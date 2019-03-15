package main

import (
	"log"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

func main() {
	var fooFlag bool
	kingpin.Flag("foo", "foo").Default("true").BoolVar(&fooFlag)
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
