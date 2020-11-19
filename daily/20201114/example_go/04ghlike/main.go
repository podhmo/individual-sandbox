package main

import (
	"flag"
	"fmt"
	"log"
	"os"
)

func main() {
	cmd := NewRouterCommand(
		os.Args[0], []*Command{
			NewIssue(),
			NewPr(),
			NewRepo(),
		},
	)

	run := func() error {
		var options struct {
			Verbose bool
		}
		cmd.BoolVar(&options.Verbose, "verbose", false, "")
		cmd.Options = options
		return cmd.Do([]*Command{cmd}, os.Args[1:])
	}

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

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

func NewCommand(name string, do func([]*Command, []string) error) *Command {
	return &Command{
		FlagSet: flag.NewFlagSet("create", flag.ExitOnError),
		Do:      do,
	}
}

// issue
func NewIssue() *Command {
	return NewRouterCommand("issue", []*Command{
		NewIssueCreate(),
		NewIssueList(),
		NewIssueStatus(),
		NewIssueView(),
	})
}

// issue create
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

// issue list
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

// issue status
func NewIssueStatus() *Command {
	return NewCommand("status", func(path []*Command, args []string) error {
		cmd := path[len(path)-1]
		if err := cmd.Parse(args); err != nil {
			return err
		}
		fmt.Println(cmd.Name(), cmd.Args())
		return nil
	})
}

// issue view
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

// pr
func NewPr() *Command {
	return NewRouterCommand("pr", []*Command{
		NewPrCheckout(),
		NewPrCreate(),
		NewPrList(),
		NewPrStatus(),
		NewPrView(),
	})
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

// pr create
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

// pr list
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

// pr status
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

// pr view
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

// repo
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

// repo clone
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

// repo create
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

// repo fork
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

// repo view
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
