package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package p

type S struct {
}

func (s *Struct) Add(x, y int) int {
	return x + y
}
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", source, parser.AllErrors)
	if err != nil {
		return err
	}
	return ast.Print(fset, f)
}
