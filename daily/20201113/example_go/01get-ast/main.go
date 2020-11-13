package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
	"strings"
)

func main() {
	targetP := flag.String("target", "", "")
	flag.Parse()

	if err := run(*targetP); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(target string) error {
	fset := token.NewFileSet()
	parts := strings.SplitN(target, ":", 2)
	filename, funcname := parts[0], parts[1]
	f, err := parser.ParseFile(fset, filename, nil, parser.ParseComments)
	if err != nil {
		return err
	}

	ob := f.Scope.Lookup(funcname)
	decl := ob.Decl.(*ast.FuncDecl)

	printer.Fprint(os.Stdout, fset, decl)
	fmt.Println("")
	printer.Fprint(os.Stdout, fset, decl.Type)
	fmt.Println("")
	for _, f := range decl.Type.Params.List {
		// printer.signature?
		printer.Fprint(os.Stdout, fset, f)
		fmt.Println("")
	}
	return nil
}
