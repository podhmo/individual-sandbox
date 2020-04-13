package hello

import (
	"flag"
	"fmt"
)

// Options ...
type Options struct {
	Name string
	Age  int
}

// NewFlagSet ...
func NewFlagSet() (*flag.FlagSet, func([]string) error) {
	cmd := flag.NewFlagSet("hello", flag.ExitOnError)
	opts := Options{}

	cmd.StringVar(&opts.Name, "name", "", "name value")
	cmd.IntVar(&opts.Age, "age", 0, "age value")

	return cmd, func(args []string) error {
		if err := cmd.Parse(args); err != nil {
			return err
		}
		return Run(opts)
	}
}

// Run ...
func Run(opts Options) error {
	fmt.Printf("Hello %s!!", opts.Name)
	return nil
}
