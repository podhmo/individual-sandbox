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
	_ = Append(config.Fset, config.Files[0], config.Files[1], "A")
	printer.Fprint(os.Stdout, config.Fset, config.Files[0])
	return nil
}

// Append :
func Append(fset *token.FileSet, x *ast.File, y *ast.File, name string) (ok bool) {
	ob := y.Scope.Lookup(name)
	if ob == nil {
		return
	}

	switch ob.Kind {
	case ast.Typ:
		// todo: comment
		x.Decls = append(x.Decls, &ast.GenDecl{
			Tok:   token.TYPE,
			Specs: []ast.Spec{ob.Decl.(ast.Spec)},
		})
		ok = true
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
