package gen

import (
	"fmt"
	"io"
	"m/validation/vm"
)

type Generator struct {
	W io.Writer
}

func (g *Generator) Generate(cmds []vm.Command) error {
	w := g.W
	for _, cmd := range cmds {
		fmt.Fprintf(w, "func Validate%s() error{\n", cmd.Name)
		fmt.Fprintln(w, "\treturn nil")
		fmt.Fprintln(w, "}")
		fmt.Fprintln(w, "")
	}
	return nil
}
