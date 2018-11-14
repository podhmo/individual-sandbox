package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package main

import (
	"log"
)

func main(){
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", source, parser.AllErrors)
	if err != nil {
		return err
	}

	ob := f.Scope.Lookup("main")
	if ob.Kind != ast.Fun {
		return fmt.Errorf("unexpected type %s", ob.Kind)
	}
	ob.Decl.(*ast.FuncDecl).Name.Name = "mainInner"

	return printer.Fprint(os.Stdout, fset, f)
}
