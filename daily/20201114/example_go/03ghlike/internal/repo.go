package internal

import (
	"flag"
	"fmt"
)

func NewRepo() *Command {
	return NewRouterCommand("repo",
		[]*Command{
			NewRepoClone(),
			NewRepoCreate(),
			NewRepoFork(),
			NewRepoView(),
		},
	)
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
