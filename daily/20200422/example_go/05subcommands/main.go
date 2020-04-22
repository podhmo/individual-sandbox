package main

import (
	"fmt"
	"log"
	"os"

	"github.com/integrii/flaggy"
)

func main() {
	parser := flaggy.NewParser("app")
	parser.Description = "main app"

	var name string
	{
		subparser := flaggy.NewSubcommand("hello")
		subparser.Description = "hello message"
		subparser.String(&name, "n", "name", "name of person")
		parser.AttachSubcommand(subparser, 1)
	}
	{
		subparser := flaggy.NewSubcommand("bye")
		subparser.Description = "bye message"
		required := true
		subparser.AddPositionalValue(&name, "name", 1, required, "name of person")
		parser.AttachSubcommand(subparser, 1)
	}

	if err := parser.ParseArgs(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
	if len(os.Args) <= 1 {
		parser.ShowHelpAndExit("subcommand is required")
	}
	fmt.Println("@")
}
