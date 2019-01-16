package main

import (
	"log"
	"os"

	"m/internal/add"
	"m/internal/hello"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

func main() {
	var err error

	switch cmd, err := kingpin.CommandLine.Parse(os.Args[1:]); cmd {
	case add.Cmd.FullCommand():
		err = add.Run()
	case hello.Cmd.FullCommand():
		err = hello.Run()
	default:
		kingpin.FatalUsage("!%+v", err)
	}

	if err != nil {
		log.Fatalf("%+v", err)
	}
}
