package main

import (
	"fmt"
	"log"
	"os"

	"m/internal/add"
	"m/internal/hello"
	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

func main() {
	app := kingpin.New("app", "sub command examples")
	commands := map[string]func() error{}
	{
		name, cont := add.SetupCommand(app)
		commands[name] = cont
	}
	{
		name, cont := hello.SetupCommand(app)
		commands[name] = cont
	}

	fullname, err := app.Parse(os.Args[1:])
	if err != nil {
		app.FatalUsage(fmt.Sprintf("!%+v", err))
	}
	cont, ok := commands[fullname]
	if !ok {
		app.FatalUsage("sub command is not found")
	}
	if err := cont(); err != nil {
		log.Fatalf("%+v", err)
	}
}
