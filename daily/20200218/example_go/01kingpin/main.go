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

	n    = Cmd.Flag("number", "number of something").Short('n').Default("0").Int()
	name = Cmd.Flag("name", "name of something").Default("").String()
)

func init() {
	Cmd.Name = "main"
	Cmd.Help = "-"
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
	n := *n
	name := *name

	fmt.Println(n, name)
	return nil
}
