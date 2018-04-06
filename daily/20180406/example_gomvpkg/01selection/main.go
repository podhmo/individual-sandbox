package main

import (
	"fmt"
	"go/ast"
	"go/types"
	"log"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package main

import (
	"fmt"
	"os"
)

type s struct{}
func (s s) Println() {}

func main(){
	fmt.Println("hello world")

	fmt := s{}
	fmt.Println()

	os.Exit(0)
}
`
	c := loader.Config{
	//TypeCheckFuncBodies: func(path string) bool { return false },
	}
	f, err := c.ParseFile("main.go", source)
	if err != nil {
		return err
	}
	c.CreateFromFiles("main", f)

	prog, err := c.Load()
	if err != nil {
		return err
	}

	info := prog.Package("main")

	var fmtpkg *types.Package
	for _, pkg := range info.Pkg.Imports() {
		if pkg.Path() == "fmt" {
			fmtpkg = pkg
		}
	}

	for _, f := range info.Files {
		// from AST
		ast.Inspect(f, func(node ast.Node) bool {
			if t, _ := node.(*ast.SelectorExpr); t != nil {
				fmt.Println(t.X, t.Sel, info.ObjectOf(t.Sel).Pkg(), fmtpkg == info.ObjectOf(t.Sel).Pkg())
			}
			return true
		})
	}
	return nil
}
