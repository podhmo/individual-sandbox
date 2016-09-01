package main

import (
	"fmt"
	"github.com/mitchellh/cli"
	"os"
)

// type cli.Command interface {
// 	Help() string
// 	Run(args []string) int
// 	Synopsis() string
// }

type sub struct{}

func (f *sub) Help() string {
	return "sub"
}

func (f *sub) Run(args []string) int {
	fmt.Println("sub")
	return 0
}

func (f *sub) Synopsis() string {
	return "Print 'sub'!"
}

func subCommandFactory() (cli.Command, error) {
	return &sub{}, nil
}

func main() {
	c := cli.NewCLI("app", "0.0.1")
	c.Args = os.Args[1:]
	c.Commands = map[string]cli.CommandFactory{
		"sub": subCommandFactory,
	}

	exitStatus, err := c.Run()
	if err != nil {
		fmt.Println(err)
	}
	os.Exit(exitStatus)
}
