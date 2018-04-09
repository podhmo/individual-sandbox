package main

import (
	"go/ast"
	"go/parser"
	"go/printer"
	"log"
	"os"

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

import "fmt"

type s struct{}

func (s s)Println(x string) {}

func main(){
	fmt.Println("xxx")

	{
		fmt := s{}
		fmt.Println("yyy")
	}

	fmt.Println("xxx")
}
`
	loader := loader.Config{ParserMode: parser.ParseComments}
	astf, err := loader.ParseFile("main.go", source)
	if err != nil {
		return err
	}
	loader.CreateFromFiles("main", astf)

	prog, err := loader.Load()
	if err != nil {
		return err
	}

	main := prog.Package("main")
	fmtpkg := prog.Package("fmt").Pkg
	for _, f := range main.Files {
		ast.Inspect(f, func(node ast.Node) bool {
			if t, _ := node.(*ast.SelectorExpr); t != nil {
				if main.Info.ObjectOf(t.Sel).Pkg() == fmtpkg {
					ast.Inspect(t.X, func(node ast.Node) bool {
						if t, _ := node.(*ast.Ident); t != nil {
							if t.Name == "fmt" && t.Obj == nil {
								t.Name = "fmt2"
							}
							return false
						}
						return true
					})
				}
				return false
			}
			return true
		})

		astutil.RewriteImport(prog.Fset, f, "fmt", "fmt2")

		pp := &printer.Config{Tabwidth: 8, Mode: printer.UseSpaces | printer.TabIndent}
		pp.Fprint(os.Stdout, prog.Fset, f)
	}
	return nil
}
