package main

import (
	"flag"
	"fmt"
	"log"
	"m/02ghlike/internal"
	"os"
)

func main() {
	cmd := &internal.Command{
		FlagSet: flag.CommandLine,
		Do: func(path []*internal.Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}

			subcmds := []*internal.Command{
				internal.NewIssue(),
				internal.NewPr(),
				internal.NewRepo(),
			}
			if cmd.NArg() == 0 {
				fmt.Fprintln(os.Stderr, "Available commands")
				for _, subcmd := range subcmds {
					fmt.Fprintf(os.Stderr, "  %s\n", subcmd.Name())
				}
				os.Exit(1)
			}
			{
				args := cmd.Args()
				for _, subcmd := range subcmds {
					if subcmd.Name() == args[0] {
						return subcmd.Do(append(path, subcmd), args[1:])
					}
				}
				return fmt.Errorf("unexpected command: %s", args[0])
			}
		},
	}

	run := func() error {
		verboseP := cmd.Bool("verbose", false, "")
		_ = verboseP
		return cmd.Do([]*internal.Command{cmd}, os.Args[1:])
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
