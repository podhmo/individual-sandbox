package internal

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
)

type Command struct {
	*flag.FlagSet
	Lookup  func([]*Command, []string) (*Command, func() error)
	Options interface{}
}

func NewRouterCommand(name string, subcmds ...*Command) *Command {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.Usage = func() {
		fmt.Fprintf(fs.Output(), "Usage of %s:\n", fs.Name())
		fs.PrintDefaults()
		fmt.Fprintln(fs.Output(), "Available commands")
		for _, sub := range subcmds {
			fmt.Fprintf(fs.Output(), "  %s\n", sub.Name())
		}
	}

	return &Command{
		FlagSet: fs,
		Lookup: func(path []*Command, args []string) (*Command, func() error) {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return cmd, func() error { return err }
			}
			if cmd.NArg() == 0 {
				return cmd, func() error {
					cmd.Usage()
					return fmt.Errorf("require command")
				}
			}

			rest := cmd.Args()
			for _, subcmd := range subcmds {
				if subcmd.Name() == rest[0] {
					return subcmd.Lookup(append(path, subcmd), rest[1:])
				}
			}
			return cmd, func() error {
				cmd.Usage()
				return fmt.Errorf("unexpected command: %s", rest[0])
			}
		},
	}
}

func MainCLI(cmd *Command, args []string) {
	run := func(args []string) (rawErr error) {
		defer func() {
			if err := recover(); err != nil {
				rawErr = fmt.Errorf("%+v", err)
			}
		}()
		cmd, action := cmd.Lookup([]*Command{cmd}, args)
		cmd.VisitAll(func(f *flag.Flag) {
			envname := strings.ToUpper(f.Name)
			if v := os.Getenv(envname); v != "" {
				if err := cmd.Set(f.Name, v); err != nil {
					panic(fmt.Sprintf("on envvar %s=%v, %v", envname, v, err))
				}
			}
		})
		return action()
	}

	log.SetFlags(0)
	if err := run(args); err != nil {
		if err != flag.ErrHelp {
			log.Fatalf("\x1b[0;33m!! %+v\x1b[0m", err)
		}
	}
}
