package gen

import (
	"io"
	"m/validation/vm"
)

type Generator struct {
	W io.Writer
}

func (g *Generator) Generate(cmds []vm.Command) error {
	for _, cmd := range cmds {

	}
}
