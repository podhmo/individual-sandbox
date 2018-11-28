package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"io/ioutil"

	"github.com/pkg/errors"
	"golang.org/x/tools/imports"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	wrapTemplate := `
package p
func main() {
	fmt.Println("start")
	defer fmt.Println("end")
	mainInner()
}
`
	// xxx: mainInner
	if len(os.Args) <= 1 {
		fmt.Fprintln(os.Stderr, "cmd <target file>")
		os.Exit(1)
	}
	targetFile := os.Args[1] // target file

	fset := token.NewFileSet()
	source, err := ioutil.ReadFile(targetFile)
	if err != nil {
		return err
	}

	log.Println("parse", targetFile)
	f, err := parser.ParseFile(fset, "main", source, parser.AllErrors)
	if err != nil {
		return errors.Wrap(err, "parse main")
	}
	f2, err := parser.ParseFile(fset, "extra", wrapTemplate, parser.AllErrors)
	if err != nil {
		return errors.Wrap(err, "parse extra")
	}

	log.Println("transform AST")

	if f.Scope.Lookup("mainInner") == nil {
		main := f.Scope.Lookup("main")
		if main.Kind != ast.Fun {
			return fmt.Errorf("unexpected type %s", main.Kind)
		}
		main.Decl.(*ast.FuncDecl).Name.Name = "mainInner"
	} else {
		main := f.Scope.Lookup("main")
		for i, decl := range f.Decls {
			if main.Decl == decl {
				f.Decls = append(f.Decls[:i], f.Decls[i+1:]...)
				fmt.Println("ok")
				break
			}
		}
	}

	fn := f2.Scope.Lookup("main").Decl.(*ast.FuncDecl)
	f.Decls = append(f.Decls, fn)

	log.Println("write", targetFile)
	buf := new(bytes.Buffer)
	if err := printer.Fprint(buf, fset, f); err != nil {
		return err
	}

	output, err := imports.Process(targetFile, buf.Bytes(), &imports.Options{
		TabWidth:  8,
		TabIndent: true,
		Comments:  true,
		Fragment:  true,
	})
	if err != nil {
		return err
	}
	return ioutil.WriteFile(targetFile, output, 0644)
}
