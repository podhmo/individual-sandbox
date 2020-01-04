package main

import (
	"fmt"
	"log"
	post "m/01multifiles/post"
	register "m/01multifiles/register"
	"os"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

var (
	// Cmd ...
	Cmd = kingpin.CommandLine
)

func init() {
	Cmd.Name = "multi commands"
	Cmd.Help = "multi commands example"
	Cmd.Version("0.0.1")
}

func main() {
	command, err := Cmd.Parse(os.Args[1:])
	if err != nil {
		Cmd.FatalUsage(fmt.Sprintf("\x1b[33m%+v\x1b[0m", err))
	}

	switch command {
	// Register user
	case register.Cmd.FullCommand():
		err = register.Run()
	// Post message
	case post.Cmd.FullCommand():
		err = post.Run()
	}

	if err != nil {
		log.Fatalf("!!%+v", err)
	}
}
