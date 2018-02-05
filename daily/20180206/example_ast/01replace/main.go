package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
)

func run() error {
	code0 := `
package p
type S struct {}
`
	code1 := `
package p
type S struct {
	Name string
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0", code0, parser.ParseComments)
	if err != nil {
		return err
	}
	f1, err := parser.ParseFile(fset, "f1", code1, parser.ParseComments)
	if err != nil {
		return err
	}

	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, fset, f0)
	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, fset, f1)
	fmt.Println("----------------------------------------")
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
