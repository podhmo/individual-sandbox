package main

import (
	"go/ast"
	"go/printer"
	"go/token"
	"log"
	"os"

	"../source"
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
	_ = Replace(config.Fset, config.Files[0], config.Files[1], "A")
	printer.Fprint(os.Stdout, config.Fset, config.Files[0])
	return nil
}

// Replace :
func Replace(fset *token.FileSet, x *ast.File, y *ast.File, name string) (ok bool) {
	// x <- y
	src := y.Scope.Lookup(name)
	if src == nil {
		return
	}
	dst := x.Scope.Lookup(name)
	if dst == nil {
		return
	}

	if src.Kind != dst.Kind {
		return // Append?
	}

	// todo: comment
	switch src.Kind {
	case ast.Typ:
		ast.Inspect(x, func(node ast.Node) bool {
			if decl, ok := node.(*ast.GenDecl); ok {
				newspec := make([]ast.Spec, len(decl.Specs))
				for i, spec := range decl.Specs {
					if spec == dst.Decl.(ast.Spec) {
						newspec[i] = src.Decl.(ast.Spec)
					} else {
						newspec[i] = spec
					}
				}
				decl.Specs = newspec
				return false
			}
			return true
		})
	}
	return
}

// const (
// 	Bad ObjKind = iota // for error handling
// 	Pkg                // package
// 	Con                // constant
// 	Typ                // type
// 	Var                // variable
// 	Fun                // function or method
// 	Lbl                // label
// )
