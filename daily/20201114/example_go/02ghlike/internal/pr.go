package internal

import (
	"flag"
	"fmt"
	"os"
)

func NewPr() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("pr", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}

			subcmds := []*Command{
				NewPrCheckout(),
				NewPrCreate(),
				NewPrList(),
				NewPrStatus(),
				NewPrView(),
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
}

func NewPrCheckout() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("checkout", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			fmt.Println(cmd.Name(), cmd.Args())
			return nil
		},
	}
}
func NewPrCreate() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("create", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			fmt.Println(cmd.Name(), cmd.Args())
			return nil
		},
	}
}
func NewPrList() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("list", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			fmt.Println(cmd.Name(), cmd.Args())
			return nil
		},
	}
}
func NewPrStatus() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("status", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			fmt.Println(cmd.Name(), cmd.Args())
			return nil
		},
	}
}
func NewPrView() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("view", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			fmt.Println(cmd.Name(), cmd.Args())
			return nil
		},
	}
}
