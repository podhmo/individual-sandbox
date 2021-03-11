package main

import (
	"fmt"
	"io"
	"os"
)

type HasName interface {
	Name() string
}

type P string

func (p P) Name() string {
	return "P"
}

func Use(w io.Writer, say func(HasName) string) {
	var p P
	fmt.Fprintln(w, say(p))
}
func main() {
	Use(
		os.Stdout,
		func(o HasName) string {
			return fmt.Sprintf("Name is %q", o.Name())
		},
	)
}
