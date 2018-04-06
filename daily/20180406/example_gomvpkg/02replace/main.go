package main

import (
	"go/ast"
	"go/printer"
	"log"
	"os"
	"strconv"

	"golang.org/x/tools/go/ast/astutil"
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
	c := loader.Config{}
	f, err := c.ParseFile("main.go", source)
	if err != nil {
		return err
	}
	c.CreateFromFiles("main", f)

	prog, err := c.Load()
	if err != nil {
		return err
	}

	// replace fmt -> fmt2
	fmtpkg := prog.Package("fmt").Pkg
	info := prog.Package("main")

	for _, f := range info.Files {
		importName := fmtpkg.Name()
		for _, is := range f.Imports {
			path, err := strconv.Unquote(is.Path.Value)
			if err != nil {
				return err
			}
			if path == "fmt" {
				if is.Name != nil {
					importName = is.Name.Name
				}
			}
		}

		// conflict check, maybe named import, skip replacement
		_ = importName

		// from AST
		ast.Inspect(f, func(node ast.Node) bool {
			if t, _ := node.(*ast.SelectorExpr); t != nil {
				if fmtpkg == info.ObjectOf(t.Sel).Pkg() {
					ast.Inspect(t.X, func(node ast.Node) bool {
						if ident, _ := node.(*ast.Ident); ident != nil {
							ident.Name = "fmt2"
						}
						return true
					})
					return false
				}
			}
			return true
		})
		astutil.RewriteImport(prog.Fset, f, "fmt", "fmt2")

		printer.Fprint(os.Stdout, prog.Fset, f)
	}
	return nil
}
