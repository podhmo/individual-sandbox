package internal

import (
	"flag"
)

type Command struct {
	*flag.FlagSet
	Do func([]*Command, []string) error
}
