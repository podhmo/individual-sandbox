package main

import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

const source = `package foo

// Foo: -
func Foo(){
	// print hello
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

	cfg := &printer.Config{Tabwidth: 8, Mode: printer.SourcePos | printer.TabIndent}
	cfg.Fprint(os.Stdout, fset, f)
	return nil
}
