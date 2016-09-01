package main

import (
	"fmt"
	"github.com/mkideal/cli"
	"os"
)

var help = cli.HelpCommand("display help information")

type rootT struct {
    cli.Helper
    Name string `cli:"name" usage:"your name"`
}

var root = &cli.Command{
    Desc: "this is root command",
    Argv: func() interface{} { return new(rootT) },
    Fn: func(ctx *cli.Context) error {
        argv := ctx.Argv().(*rootT)
        ctx.String("Hello, root command, I am %s\n", argv.Name)
        return nil
    },
}

type subT struct {
    cli.Helper
    Name string `cli:"name" usage:"your name"`
}

var sub = &cli.Command{
    Name: "sub",
    Desc: "this is a sub command",
    Argv: func() interface{} { return new(subT) },
    Fn: func(ctx *cli.Context) error {
        argv := ctx.Argv().(*subT)
        ctx.String("Hello, sub command, I am %s\n", argv.Name)
        return nil
    },
}

func main() {
	if err := cli.Root(root,
		cli.Tree(help),
		cli.Tree(sub),
	).Run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
