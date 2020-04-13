package byebye

import (
	"flag"
	"fmt"
)

// Options ...
type Options struct {
	Name string
}

// NewFlagSet ...
func NewFlagSet() (*flag.FlagSet, func([]string) error) {
	cmd := flag.NewFlagSet("byebye", flag.ExitOnError)
	opts := Options{}

	cmd.StringVar(&opts.Name, "name", "", "name value")
	return cmd, func(args []string) error {
		if err := cmd.Parse(args); err != nil {
			return err
		}
		return Run(opts)
	}
}

// Run ...
func Run(opts Options) error {
	fmt.Printf("Byebye %s!!", opts.Name)
	return nil
}
