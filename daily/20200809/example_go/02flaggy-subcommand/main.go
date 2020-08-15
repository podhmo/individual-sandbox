package main

import (
	"log"
	"os"

	"m/02flaggy-subcommand/bye"
	"m/02flaggy-subcommand/hello"

	"github.com/integrii/flaggy"
)

func main() {
	parser := flaggy.NewParser("app")
	parser.Description = "main app"

	subcommands := map[*flaggy.Subcommand]func() error{
		hello.Commandline: hello.Run,
		bye.Commandline:   bye.Run,
	}
	for subcmd := range subcommands {
		parser.AttachSubcommand(subcmd, 1)
	}

	if err := parser.ParseArgs(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
	if len(os.Args) <= 1 {
		parser.ShowHelpAndExit("subcommand is required")
	}

	for subcmd, action := range subcommands {
		if subcmd.Used {
			if err := action(); err != nil {
				log.Fatalf("!!!%+v", err)
			}
			break
		}
	}
}
