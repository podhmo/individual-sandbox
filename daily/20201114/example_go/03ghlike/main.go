package main

import (
	"log"
	"m/03ghlike/internal"
	"os"
)

func main() {
	cmd := internal.NewRouterCommand(
		os.Args[0], []*internal.Command{
			internal.NewIssue(),
			internal.NewPr(),
			internal.NewRepo(),
		},
	)

	run := func() error {
		var options struct {
			Verbose bool
		}
		cmd.BoolVar(&options.Verbose, "verbose", false, "")
		cmd.Options = options
		return cmd.Do([]*internal.Command{cmd}, os.Args[1:])
	}

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
