package main

import (
	"github.com/ukautz/clif"
)

func main() {
	c := clif.New("app", "0.0.0", "root")

	sub := clif.NewCommand("sub", "sub command", func(c *clif.Command, out clif.Output) {
		name := c.Argument("name").String()
		out.Printf("sub -- %s: hello\n", name)
	}).NewArgument("name", "name for greeting", "", true, false)

	c.Add(sub)
	c.Run()
}
