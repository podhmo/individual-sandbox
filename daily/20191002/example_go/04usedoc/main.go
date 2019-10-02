package main

import (
	"errors"
	"fmt"
	"go/ast"
	"go/doc"
	"go/format"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(args []string) error {
	fmt.Fprintln(os.Stderr, "args", args)
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		// Mode: packages.LoadAllSyntax,
		Mode: packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes,
	}
	pkgs, err := packages.Load(cfg, args...)
	if err != nil {
		return err
	}

	packages.PrintErrors(pkgs)

	for _, pkg := range pkgs {
		fmt.Fprintln(os.Stderr, pkg.ID)
		p := &ast.Package{
			Name:    pkg.ID,
			Imports: nil,
			Files:   map[string]*ast.File{},
		}
		for _, f := range pkg.Syntax {
			if p.Scope == nil {
				p.Scope = f.Scope.Outer
			} else if p.Scope != f.Scope.Outer {
				return errors.New("mismatch scope?")
			}

			p.Files[fileName(fset, f)] = f
		}

		dp := doc.New(p,
			pkg.ID,
			doc.AllMethods,
			//doc.PreserveAST,
		)

		for _, typ := range dp.Types {
			for _, method := range typ.Methods {
				if err := format.Node(os.Stdout, fset, method.Decl); err != nil {
					return err
				}
				fmt.Println("")
			}
		}
	}

	return nil
}

func fileName(fset *token.FileSet, t *ast.File) string {
	f := fset.File(t.Pos())
	if f == nil {
		return "-"
	}
	return f.Name()
}
