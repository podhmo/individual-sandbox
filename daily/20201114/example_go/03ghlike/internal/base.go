package internal

import (
	"flag"
	"fmt"
	"os"
)

type Command struct {
	*flag.FlagSet
	Do      func([]*Command, []string) error
	Options interface{}
}

func NewRouterCommand(name string, subcmds []*Command) *Command {
	return &Command{
		FlagSet: flag.NewFlagSet(name, flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}

			if cmd.NArg() == 0 {
				fmt.Fprintln(cmd.Output(), "Available commands")
				for _, subcmd := range subcmds {
					fmt.Fprintf(cmd.Output(), "  %s\n", subcmd.Name())
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
				fmt.Fprintf(cmd.Output(), "unexpected command: %s", args[0])
				os.Exit(1)
				return nil // never
			}
		},
	}
}
