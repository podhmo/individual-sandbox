package main

import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

const (
	source = `
package p

// F :
func F() int {
	return 1 + 1
}
`
)

func main() {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)

	if err != nil {
		log.Fatal(err)
	}

	printer.Fprint(os.Stdout, fset, f)

	fset.File(f.Pos()).SetLines(nil)
	fmt.Println("----------------------------------------")

	printer.Fprint(os.Stdout, fset, f)
}
