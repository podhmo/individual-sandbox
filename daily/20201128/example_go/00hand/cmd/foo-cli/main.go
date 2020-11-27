package main

import (
	"bytes"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"m/00hand/handler"
	"net/url"
	"os"
)

func main() {
	cmd := NewRouterCommand(
		os.Args[0], []*Command{
			NewHello(),
			NewIsEven(),
		},
	)

	run := func() error {
		return cmd.Do([]*Command{cmd}, os.Args[1:])
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func NewHello() *Command {
	fs := flag.NewFlagSet("Hello", flag.ExitOnError)
	var options struct {
		Name string `json:"name"`
	}
	fs.StringVar(&options.Name, "name", "", "")

	return &Command{
		FlagSet: fs,
		Options: &options,
		Do:      CLIAdapter(handler.Hello),
	}
}

func NewIsEven() *Command {
	fs := flag.NewFlagSet("IsEven", flag.ExitOnError)
	var v int
	fs.IntVar(&v, "v", 0, "")

	return &Command{
		FlagSet: fs,
		Options: &v,
		Do:      CLIAdapter(handler.IsEven),
	}
}

// ---
type DoFunc func(path []*Command, args []string) error

func CLIAdapter(h handler.HandlerFunc) DoFunc {
	return func(path []*Command, args []string) error {
		cmd := path[len(path)-1]
		if err := cmd.Parse(args); err != nil {
			return err
		}

		name := cmd.Name()

		var b bytes.Buffer
		enc := json.NewEncoder(&b)
		if err := enc.Encode(cmd.Options); err != nil {
			return err
		}

		var dummy url.Values
		ev := handler.Event{
			Name:    name,
			Body:    ioutil.NopCloser(&b),
			Headers: dummy,
		}

		ctx := context.Background()
		result, err := h(ctx, ev)
		if err != nil {
			return err
		}

		{
			enc := json.NewEncoder(os.Stdout)
			enc.SetIndent("", "  ")
			return enc.Encode(result)
		}
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
