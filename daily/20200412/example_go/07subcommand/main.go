package main

import (
	"flag"
	"fmt"
	"log"
	"m/byebye"
	"m/hello"
	"os"
)

func main() {
	cmd := flag.NewFlagSet("main", flag.ContinueOnError)

	name := ""
	actions := map[string]func([]string) error{}

	// hello
	{
		subcmd, action := hello.NewFlagSet()
		cmd.StringVar(&name, subcmd.Name(), "", "HelloCommand")
		actions[subcmd.Name()] = action
	}
	// bybye
	{
		subcmd, action := byebye.NewFlagSet()
		cmd.StringVar(&name, subcmd.Name(), "", "ByebyeCommand")
		actions[subcmd.Name()] = action
	}

	cmd.Usage = func() {
		o := cmd.Output()
		fmt.Fprintf(o, "Usage: %s <command>\n", cmd.Name())
		fmt.Fprintln(o)
		fmt.Fprintln(o, "where <command> is one of:")
		cmd.VisitAll(func(f *flag.Flag) {
			fmt.Fprintf(o, "  %s\n", f.Name)
			fmt.Fprintf(o, "    \t%s\n", f.Usage)
		})

		fmt.Fprintln(o)
		fmt.Fprintln(o, "program <command> -h for subcommand help")
		os.Exit(1)
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

	if err := actions[name](os.Args[2:]); err != nil {
		log.Fatalf("!+%v", err)
	}
}
