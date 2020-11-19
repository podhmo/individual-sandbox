package internal

import (
	"flag"
	"fmt"
)

func NewIssue() *Command {
	return NewRouterCommand("issue", []*Command{
		NewIssueCreate(),
		NewIssueList(),
		NewIssueStatus(),
		NewIssueView(),
	})
}

func NewIssueCreate() *Command {
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
func NewIssueList() *Command {
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
func NewIssueStatus() *Command {
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
func NewIssueView() *Command {
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
