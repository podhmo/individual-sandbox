package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/k0kubun/pp"
)

type ChoicesValue struct {
	value        string
	Choices      []string
	Descriptions []string
}

func (v *ChoicesValue) Add(name, description string) {
	v.Choices = append(v.Choices, name)
	v.Descriptions = append(v.Descriptions, name)
}
func (v *ChoicesValue) String() string {
	return fmt.Sprintf("%+v", v.value)
}
func (v *ChoicesValue) Set(s string) error {
	s = strings.TrimSpace(s)
	for _, c := range v.Choices {
		if c == s {
			v.value = s
			return nil
		}
	}
	err := fmt.Errorf("%q is not in %#+v", s, v.Choices)
	return err
}
func (v *ChoicesValue) Get() interface{} {
	return v.value
}

func main() {
	cmd := flag.NewFlagSet("main", flag.ContinueOnError)
	n := cmd.Int64("n", 0, "*n*")

	subcommands := &ChoicesValue{}
	subcommands.Add("foo", "Foo command")
	subcommands.Add("bar", "Bar command")

	if err := subcommands.Set(os.Args[1]); err != nil {
		o := cmd.Output()
		fmt.Fprintf(o, "Usage: %s <command>\n", cmd.Name())
		fmt.Fprintln(o)
		fmt.Fprintln(o, "where <command> is one of:")
		for i, name := range subcommands.Choices {
			fmt.Fprintln(o, "  ", name, subcommands.Descriptions[i])
		}
		fmt.Fprintln(o)
		fmt.Fprintln(o, "available flags:")
		cmd.PrintDefaults()
		fmt.Fprintln(o)
		fmt.Fprintln(o, "program <command> -h for subcommand help")
		os.Exit(1)
	}

	fmt.Println(cmd.Parse(os.Args[1:]))
	pp.Println(subcommands, n)
}
