package main

import (
	"go/ast"
	"go/printer"
	"go/token"
	"log"
	"os"
	"strings"

	"../source"
	"golang.org/x/tools/go/ast/astutil"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %#v\n", err)
	}
}

func run() error {
	config, err := source.Build()
	if err != nil {
		return err
	}

	// merge
	t := Merge(config.Fset, config.Files[0], config.Files[1])
	printer.Fprint(os.Stdout, config.Fset, t)
	return nil
}

// Merge :
func Merge(fset *token.FileSet, x *ast.File, y *ast.File) *ast.File {
	r := *x
	// merge import
	importIndex := 0
	for i, decl := range y.Decls {
		if decl, ok := decl.(*ast.GenDecl); ok {
			if decl.Tok == token.IMPORT {
				for _, spec := range decl.Specs {
					if ispec, ok := spec.(*ast.ImportSpec); ok {
						// r.Importsを真面目に見ても良いけれど
						astutil.AddImport(fset, &r, strings.Trim(ispec.Path.Value, `"`))
					}
				}
				importIndex = i + 1
				break
			}
		}
	}
	r.Decls = append(r.Decls, y.Decls[importIndex:]...)
	return &r
}
