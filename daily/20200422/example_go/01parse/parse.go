package parse

import (
	"flag"
	"strings"
)

type Option struct {
	Name string // required
}

func New(opt *Option) *flag.FlagSet {
	cmd := flag.NewFlagSet("app", flag.ContinueOnError)
	cmd.StringVar(&opt.Name, "name", "", "<name>")
	cmd.StringVar(&opt.Name, "n", "", "<name> (shorthand)")

	var b strings.Builder
	cmd.SetOutput(&b)
	return cmd
}
