package main

import (
	"fmt"

	"github.com/integrii/flaggy"
)

func main() {
	flaggy.SetName("app")
	flaggy.SetDescription("main app")

	{
		subparser := flaggy.NewSubcommand("hello")
		subparser.Description = "hello message"
		flaggy.AttachSubcommand(subparser, 1)
	}
	{
		subparser := flaggy.NewSubcommand("bye")
		subparser.Description = "bye message"
		flaggy.AttachSubcommand(subparser, 1)
	}
	flaggy.Parse()
	fmt.Println("@")
}
