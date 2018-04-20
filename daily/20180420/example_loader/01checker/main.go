package main

import (
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
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

import "fmt"

type S struct {}
func (s *S) Hello() { fmt.Println("Hello") }
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "s.go", source, parser.ParseComments)
	if err != nil {
		return err
	}
	c := &types.Config{
		Importer: importer.Default(),
	}
	checker := types.NewChecker(c, fset, types.NewPackage("p", "p"), nil)
	return checker.Files([]*ast.File{f})
}
