package main

import (
	"fmt"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(pattern []string) error {
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, pattern...)
	if err != nil {
		return fmt.Errorf("packages, load %w", err)
	}
	for _, pkg := range pkgs {
		for _, f := range pkg.Syntax {
			for name := range f.Scope.Objects {
				fmt.Println(name)
			}
		}
	}
	return nil
}
