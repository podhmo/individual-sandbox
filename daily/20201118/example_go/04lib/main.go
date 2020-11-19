package main

import (
	"flag"
	"fmt"
	"m/04lib/internal"
	"os"
)

func main() {
	internal.MainCLI(
		internal.NewRouterCommand(
			os.Args[0],
			NewIssue(),
			NewFoo(),
		),
		os.Args[1:],
	)
}

// issue
func NewIssue() *internal.Command {
	return internal.NewRouterCommand("issue",
		NewIssueCreate(),
		NewIssueList(),
	)
}

// issue create
func NewIssueCreate() *internal.Command {
	var options struct {
		title string
	}
	fs := flag.NewFlagSet("create", flag.ContinueOnError)
	fs.StringVar(&options.title, "title", "", "title of issue")

	return &internal.Command{
		FlagSet: fs,
		Lookup: func(path []*internal.Command, args []string) (*internal.Command, func() error) {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return cmd, func() error { return err }
			}
			return cmd, func() error {
				fmt.Println(cmd.Name(), cmd.Args(), "name is", options.title)
				return nil
			}
		},
		Options: options,
	}
}

// issue list
func NewIssueList() *internal.Command {
	var options struct{}
	fs := flag.NewFlagSet("list", flag.ContinueOnError)
	return &internal.Command{
		FlagSet: fs,
		Lookup: func(path []*internal.Command, args []string) (*internal.Command, func() error) {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return cmd, func() error { return err }
			}
			return cmd, func() error {
				fmt.Println(cmd.Name(), cmd.Args())
				return nil
			}
		},
		Options: options,
	}
}

// foo
func NewFoo() *internal.Command {
	fs := flag.NewFlagSet("foo", flag.ContinueOnError)
	var options struct {
		title string
		age   uint
	}
	fs.StringVar(&options.title, "title", "", "title of foo")
	fs.UintVar(&options.age, "age", 0, "age of foo")

	return &internal.Command{
		FlagSet: fs,
		Lookup: func(path []*internal.Command, args []string) (*internal.Command, func() error) {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return cmd, func() error { return err }
			}
			return cmd, func() error {
				fmt.Println(cmd.Name(), cmd.Args(), "name is", options.title)
				return nil
			}
		},
		Options: options,
	}
}
