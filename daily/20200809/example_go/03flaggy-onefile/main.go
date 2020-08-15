package main

import (
	"fmt"
	"log"
	"os"

	"github.com/integrii/flaggy"
)

type App struct {
	Cmd *flaggy.Subcommand
	Run func() error
}

func NewHelloApp() *App {
	var name string

	cmd := flaggy.NewSubcommand("hello")
	cmd.Description = "hello message"
	cmd.String(&name, "n", "name", "name of person")

	return &App{
		Cmd: cmd,
		Run: func() error {
			fmt.Println("Hello", name)
			return nil
		},
	}
}

func NewByebyeApp() *App {
	var name string

	cmd := flaggy.NewSubcommand("byebye")
	cmd.Description = "byebye message"
	cmd.String(&name, "n", "name", "name of person")

	return &App{
		Cmd: cmd,
		Run: func() error {
			fmt.Println("Byebye", name)
			return nil
		},
	}
}

func main() {
	parser := flaggy.NewParser("app")
	parser.Description = "main app"

	apps := []*App{
		NewHelloApp(),
		NewByebyeApp(),
	}
	for _, app := range apps {
		parser.AttachSubcommand(app.Cmd, 1)
	}

	if err := parser.ParseArgs(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
	if len(os.Args) <= 1 {
		parser.ShowHelpAndExit("subcommand is required")
	}

	for _, app := range apps {
		if app.Cmd.Used {
			if err := app.Run(); err != nil {
				log.Fatalf("!!!%+v", err)
			}
			break
		}
	}
}
