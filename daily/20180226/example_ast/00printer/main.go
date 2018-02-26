package main

import (
	"bytes"
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"strings"
)

func main() {
	code := `
package p

// F :
func F() int {
	x := 5
	return x + x
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f.go", code, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	{
		fmt.Println(fset.Position(f0.Comments[0].Pos()))
		var b bytes.Buffer
		printer.Fprint(&b, fset, f0)
		for i, line := range strings.Split(b.String(), "\n") {
			fmt.Println(i+1, line)
		}
	}

	fmt.Println("----------------------------------------")
	f0.Comments[0].List[0].Slash = token.NoPos

	{
		fmt.Println(fset.Position(f0.Comments[0].Pos()))
		var b bytes.Buffer
		printer.Fprint(&b, fset, f0)
		for i, line := range strings.Split(b.String(), "\n") {
			fmt.Println(i+1, line)
		}
	}
}
