package main

import (
	"flag"
	"fmt"
)

type Command struct {
	*flag.FlagSet
	Action func(flagSet *flag.FlagSet, args []string) error
}

func main() {
	flag.Parse()
	args := flag.Args()

	match := func() *Command {
		commands := []*Command{
			{FlagSet: flag.NewFlagSet("hello", flag.ExitOnError), Action: Hello},
			{FlagSet: flag.NewFlagSet("byebye", flag.ExitOnError), Action: Byebye},
		}
		for _, cmd := range commands {
			if args[0] == cmd.Name() {
				return cmd
			}
		}
		return nil
	}()
	if match == nil {
		fmt.Println("unexpected", args[0])
		return
	}
	match.Action(match.FlagSet, args[1:])
}

func Hello(args []string) error {
	return nil
}

func Byebye(args []string) error {
	return nil
}
