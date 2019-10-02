package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
)

func main() {
	fmt.Println(run())
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f.go", code, parser.ParseComments)
	if err != nil {
		return err
	}

	printer.Fprint(os.Stdout, fset, f.Decls[0])
	fmt.Println("\n----------------------------------------")
	printer.Fprint(os.Stdout, fset, f.Decls[0].(*ast.FuncDecl).Type)
	fmt.Println("\n----------------------------------------")
	printer.Fprint(os.Stdout, fset, f.Decls[0].(*ast.FuncDecl).Body)
	return nil
}

const code string = `
package main

// F :
// - xxx
func F(n int) string {
	if n == 0 {
		return "<zero>"
	}
	return fmt.Sprintf("<N%d>", n)
}
`
