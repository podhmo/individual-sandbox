package x00

import (
	conf "github.com/podhmo/apikit/conf"
	foo "github.com/podhmo/apikit/foo"
)

func run(filename string) error {
	c, err := conf.LoadConfig(filename)
	if err != nil {
		return err
	}

	foo := foo.FromConfig(c)
	// context?
	use(foo)
}
