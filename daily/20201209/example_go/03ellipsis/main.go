package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

const code = `package foo
func Sum(xs ...int) int {
	return 0
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "foo.go", code, parser.ParseComments)
	if err != nil {
		return err
	}
	ast.Print(fset, f)
	return nil
}
