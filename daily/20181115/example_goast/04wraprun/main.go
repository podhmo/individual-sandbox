package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"io/ioutil"

	"github.com/pkg/errors"
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
	f, err := parser.ParseFile(fset, "main", source, parser.AllErrors)
	if err != nil {
		return errors.Wrap(err, "parse main")
	}
	f2, err := parser.ParseFile(fset, "extra", wrapTemplate, parser.AllErrors)
	if err != nil {
		return errors.Wrap(err, "parse extra")
	}

	ob := f.Scope.Lookup("main")
	if ob.Kind != ast.Fun {
		return fmt.Errorf("unexpected type %s", ob.Kind)
	}
	ob.Decl.(*ast.FuncDecl).Name.Name = "mainInner"

	fn := f2.Scope.Lookup("main").Decl.(*ast.FuncDecl)
	f.Decls = append(f.Decls, fn)

	buf := new(bytes.Buffer)
	// buf := os.Stdout
	if err := printer.Fprint(buf, fset, f); err != nil {
		return err
	}

	output, err := format.Source(buf.Bytes())
	if err != nil {
		return err
	}
	os.Stdout.Write(output)
	return nil
}
