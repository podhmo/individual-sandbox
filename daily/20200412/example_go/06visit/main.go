package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {
	cmd := flag.NewFlagSet("main", flag.ContinueOnError)

	subcommand := ""
	cmd.StringVar(&subcommand, "foo", "", "FooCommand")
	cmd.StringVar(&subcommand, "bar", "", "BarCommand")

	cmdFlags := flag.NewFlagSet("main", flag.ContinueOnError)

	var n int64
	cmdFlags.Int64Var(&n, "int64", 0, "int64")
	cmdFlags.Int64Var(&n, "n", 0, "(short option of --int64)")

	cmd.Usage = func() {
		o := cmd.Output()
		fmt.Fprintf(o, "Usage: %s <command>\n", cmd.Name())
		fmt.Fprintln(o)
		fmt.Fprintln(o, "where <command> is one of:")
		cmd.VisitAll(func(f *flag.Flag) {
			fmt.Fprintln(o, "  ", f.Name, f.Usage)
		})

		fmt.Fprintln(o)
		fmt.Fprintln(o, "available flags:")
		cmdFlags.PrintDefaults()

		fmt.Fprintln(o)
		fmt.Fprintln(o, "program <command> -h for subcommand help")
		os.Exit(0)
	}

	if len(os.Args) < 2 {
		cmd.Usage()
	}

	err := fmt.Errorf(`invalid value %q for <command>: not found`, os.Args[1])
	cmd.VisitAll(func(f *flag.Flag) {
		if f.Name == os.Args[1] {
			err = f.Value.Set(os.Args[1])
		}
	})
	if err != nil {
		fmt.Fprintln(cmd.Output(), err)
		cmd.Usage()
	}
	if err := cmdFlags.Parse(os.Args[2:]); err != nil {
		os.Exit(0)
	}
	fmt.Println(subcommand)
}
