package cmd

import (
	"fmt"
	"github.com/mitchellh/cli"
)

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

func init() {
	RootCmd.AddCommand("sub", subCommandFactory)
}
