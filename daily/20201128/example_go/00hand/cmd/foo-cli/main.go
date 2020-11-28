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
	"strings"
)

func main() {
	cmd := NewRouterCommand(
		os.Args[0], []*Command{
			NewHello(),
			NewIsEven(),
			NewAddTodo(),
			NewListTodo(),
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
func NewAddTodo() *Command {
	fs := flag.NewFlagSet("AddTodo", flag.ExitOnError)
	var options struct {
		TodoFileName string          `json:"-"`
		Todo         json.RawMessage `json:"todo"`
	}
	fs.StringVar(&options.TodoFileName, "todo", "", "json-string or @<filename>")

	return &Command{
		FlagSet: fs,
		Options: &options,
		Do: CLIAdapter(handler.AddTodo, func() error {
			if !strings.HasPrefix(options.TodoFileName, "@") {
				options.Todo = json.RawMessage(options.TodoFileName)
			} else {
				b, err := ioutil.ReadFile(strings.TrimPrefix(options.TodoFileName, "@"))
				if err != nil {
					return err
				}
				options.Todo = json.RawMessage(b)
			}
			return nil
		}),
	}
}
func NewListTodo() *Command {
	fs := flag.NewFlagSet("ListTodo", flag.ExitOnError)
	var options struct {
	}
	return &Command{
		FlagSet: fs,
		Options: &options,
		Do:      CLIAdapter(handler.ListTodo),
	}
}

// ---
type DoFunc func(path []*Command, args []string) error

func CLIAdapter(h handler.HandlerFunc, callbacks ...func() error) DoFunc {
	return func(path []*Command, args []string) error {
		cmd := path[len(path)-1]
		if err := cmd.Parse(args); err != nil {
			return err
		}
		for _, cb := range callbacks {
			if err := cb(); err != nil {
				return err
			}
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
	fs := flag.NewFlagSet(name, flag.ExitOnError)
	fs.Usage = func() {
		fmt.Fprintf(fs.Output(), "Usage of %s:\n\n", name)
		fs.PrintDefaults()
		fmt.Fprintln(fs.Output(), "Available commands")
		for _, subcmd := range subcmds {
			fmt.Fprintf(fs.Output(), "  %s\n", subcmd.Name())
		}
	}
	return &Command{
		FlagSet: fs,
		Do: func(path []*Command, args []string) error {
			cmd := path[len(path)-1]
			if err := cmd.Parse(args); err != nil {
				return err
			}
			if cmd.NArg() == 0 {
				cmd.Usage()
				os.Exit(1)
				return nil
			}

			{
				args := cmd.Args()
				for _, subcmd := range subcmds {
					if subcmd.Name() == args[0] {
						return subcmd.Do(append(path, subcmd), args[1:])
					}
				}

				cmd.Usage()
				fmt.Fprintf(cmd.Output(), "\nunexpected command: %s\n", args[0])
				os.Exit(1)
				return nil
			}
		},
	}
}
