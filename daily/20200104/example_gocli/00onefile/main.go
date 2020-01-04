package main

import (
	"fmt"
	"log"
	"os"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

var (
	// Cmd ...
	Cmd = kingpin.CommandLine

	debug = Cmd.Flag("debug", "Enable debug mode.").Bool()
	name  = Cmd.Arg("name", "name").Required().String()
)

func init() {
	Cmd.Name = "hello world"
	Cmd.Help = "hello world example"
	Cmd.Version("0.0.1")
}

func main() {
	_, err := Cmd.Parse(os.Args[1:])
	if err != nil {
		Cmd.FatalUsage(fmt.Sprintf("\x1b[33m%+v\x1b[0m", err))
	}
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	name := *name

	fmt.Println("hello", name)
	return nil
}
