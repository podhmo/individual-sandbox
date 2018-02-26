package main

import (
	"bytes"
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"strings"

	"github.com/podhmo/astknife/action/replace"
)

func main() {
	code0 := `
package p

// F :
func F() int {
	return 10
}

// G :
func G() int {
	return 20
}
`
	code1 := `
package p

// F :
func F() int {
	x := 5
	return x + x
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0.go", code0, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	f1, err := parser.ParseFile(fset, "f1.go", code1, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	if _, err := replace.ToplevelToFile(f0, f0.Scope.Lookup("F"), f1.Scope.Lookup("F")); err != nil {
		log.Fatal(err)
	}

	for _, cg := range f0.Comments {
		fmt.Println(fset.Position(cg.Pos()))
	}
	// ast.Fprint(&b, fset, f0, nil)
	var b bytes.Buffer
	printer.Fprint(&b, fset, f0)
	for i, line := range strings.Split(b.String(), "\n") {
		fmt.Printf("%02d: %s\n", i+1, line)
	}
}
