package main

import (
	"./cmd"
	"fmt"
	"github.com/mitchellh/cli"
	"os"
)

func main() {
	c := cli.NewCLI("app", "0.0.1")
	c.Args = os.Args[1:]
	c.Commands = cmd.RootCmd.Build()

	exitStatus, err := c.Run()
	if err != nil {
		fmt.Println(err)
	}
	os.Exit(exitStatus)
}
