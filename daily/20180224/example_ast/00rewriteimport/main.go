package main

import (
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
)

func main() {
	code := `
package p
import (
	"errors"
)
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "p.go", code, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	astutil.RewriteImport(fset, f, "errors", "github/pkg/errors")

	printer.Fprint(os.Stdout, fset, f)
}
