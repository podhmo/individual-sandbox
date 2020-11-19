package internal

import (
	"flag"
	"fmt"
	"os"
)

func NewRepo() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("repo", flag.ExitOnError),
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}

			subcmds := []*Command{
				NewRepoClone(),
				NewRepoCreate(),
				NewRepoFork(),
				NewRepoView(),
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

func NewRepoClone() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("clone", flag.ExitOnError),
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
func NewRepoCreate() *Command {
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
func NewRepoFork() *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("fork", flag.ExitOnError),
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
func NewRepoView() *Command {
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
