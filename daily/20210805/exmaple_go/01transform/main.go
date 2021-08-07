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

func Mark(s string, args ...interface{}) {}

const source = `package foo

// Foo: -
func Foo(){
	// print hello
	Mark("xxx")
	fmt.Println("Hello")
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	fset := token.NewFileSet()
	// f, err := parser.ParseExprFrom(fset, "p", source, parser.ParseComments|parser.AllErrors)
	f, err := parser.ParseFile(fset, "p", source, parser.ParseComments|parser.AllErrors)
	if err != nil {
		return fmt.Errorf("parse %w", err)
	}

	ast.Fprint(os.Stdout, fset, f, nil)
	ast.Inspect(f, func(node ast.Node) (cont bool) {
		switch t := node.(type) {
		case *ast.CallExpr:
			var name string
			switch fun := t.Fun.(type) {
			case *ast.Ident:
				name = fun.Name
			case *ast.SelectorExpr:
				name = fun.Sel.Name // TODO: fun.X
			default:
				panic(fmt.Sprintf("%T %+v", t.Fun, fun))
			}

			if name == "Mark" {
				fmt.Println("@@", name, "len(args)=", len(t.Args))
			} else {
				fmt.Println("!!", name, "len(args)=", len(t.Args))
			}
			return false
		}
		return true
	})

	cfg := &printer.Config{Tabwidth: 8, Mode: printer.SourcePos | printer.TabIndent}
	cfg.Fprint(os.Stdout, fset, f)

	return nil
}
