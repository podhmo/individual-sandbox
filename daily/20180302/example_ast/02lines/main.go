package main

import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
	"reflect"

	"golang.org/x/tools/go/loader"
)

// toplevel comment

// main :
func main() {
	fset := token.NewFileSet()
	config := loader.Config{Fset: fset, ParserMode: parser.ParseComments}
	config.Import(".")
	prog, err := config.Load()
	if err != nil {
		log.Fatal(err)
	}

	// get *ast.File
	f := prog.Package(".").Files[0]

	// get *token.File from *ast.File
	tokenf := fset.File(f.Pos())
	fmt.Println("lines=", reflect.ValueOf(tokenf).Elem().FieldByName("lines"))

	// remove lines information
	fset.File(f.Pos()).SetLines(nil)
	fmt.Println("----------------------------------------")

	printer.Fprint(os.Stdout, fset, f)
}
