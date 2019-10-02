package main

import (
	"fmt"
	"go/ast"
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
		for _, f := range pkg.Syntax {
			if err := ast.Print(fset, f); err != nil {
				return err
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
