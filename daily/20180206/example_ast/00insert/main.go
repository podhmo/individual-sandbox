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
func (s *S) Hello() string {
	return "hello"
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
	// ast.Print(fset, f0) // decl and scope
	printer.Fprint(os.Stdout, fset, f0)
	fmt.Println("----------------------------------------")

	astutil.AddImport(fset, f1, "time.Time")
	// ast.Print(fset, f1) // decl and scope
	printer.Fprint(os.Stdout, fset, f1)
	fmt.Println("----------------------------------------")
	fmt.Println("*merged*")
	fmt.Println("----------------------------------------")

	f2 := MergeFile(f0, f1)
	printer.Fprint(os.Stdout, fset, f2)
	return nil
}

// MergeFile :
func MergeFile(x *ast.File, y *ast.File) *ast.File {
	z := *x
	z.Decls = append(x.Decls, y.Decls...)
	z.Imports = append(x.Imports, y.Imports...)
	z.Unresolved = append(x.Unresolved, y.Unresolved...)
	z.Comments = append(x.Comments, y.Comments...)
	return &z
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
