package main

import (
	"bytes"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"m/00hand/action"
	"m/00hand/handler"
	"net/url"
	"os"
	"strconv"
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
		UserFileName string          `json:"-"`
		User         json.RawMessage `json:"user"`
	}
	fs.StringVar(&options.UserFileName, "user", "", "")

	return &Command{
		FlagSet: fs,
		Options: &options,
		Do: CLIAdapter(handler.Hello, func() (err error) {
			options.User, err = ExtractRawMessageFromFile(options.UserFileName)
			if err != nil {
				return err
			}
			return nil
		}),
	}
}

func NewIsEven() *Command {
	fs := flag.NewFlagSet("IsEven", flag.ExitOnError)
	var options struct {
		V int `json:"v"`
	}
	fs.IntVar(&options.V, "v", 0, "")

	return &Command{
		FlagSet: fs,
		Options: &options,
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
func ExtractRawMessageFromFile(filenameOrContent string) (json.RawMessage, error) {
	if filenameOrContent == "" {
		return json.RawMessage(`{}`), nil
	}
	if !strings.HasPrefix(filenameOrContent, "@") {
		return json.RawMessage(filenameOrContent), nil
	}

	b, err := ioutil.ReadFile(strings.TrimPrefix(filenameOrContent, "@"))
	if err != nil {
		return nil, err
	}
	return json.RawMessage(b), nil
}

type DoFunc func(path []*Command, args []string) error

func CLIAdapter(h handler.HandlerFunc, callbacks ...func() error) DoFunc {
	return func(path []*Command, args []string) error {
		cmd := path[len(path)-1]
		if err := cmd.Parse(args); err != nil {
			return err
		}
		for _, cb := range callbacks {
			if err := cb(); err != nil {
				cmd.Usage()
				return err
			}
		}
		name := cmd.Name()

		var b bytes.Buffer
		enc := json.NewEncoder(&b)
		if err := enc.Encode(cmd.Options); err != nil {
			cmd.Usage()
			return err
		}

		if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
			fmt.Fprintln(os.Stderr, "----------------------------------------")
			fmt.Fprint(os.Stderr, "data: ", b.String())
			fmt.Fprintln(os.Stderr, "----------------------------------------")
		}

		var dummy url.Values
		ev := handler.Event{
			Name:    name,
			Body:    ioutil.NopCloser(&b), // TODO: DEBUG=1 show data
			Headers: dummy,
		}

		ctx := context.Background()
		ctx = action.SetupContext(ctx)
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
