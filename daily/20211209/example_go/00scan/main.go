package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"reflect"

	"golang.org/x/tools/go/packages"
)

func main() {
	// var mux http.ServeMux
	// rt := reflect.TypeOf(mux)
	rt := reflect.TypeOf(ast.File{})
	fmt.Println(rt.PkgPath())

	cfg := &packages.Config{
		Fset: token.NewFileSet(),
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, rt.PkgPath())
	if err != nil {
		panic(err)
	}
	fmt.Println(packages.PrintErrors(pkgs))
	fmt.Println("----------------------------------------")

	for _, pkg := range pkgs {
		fmt.Println(pkg)
		fmt.Println(pkg.Name)
		fmt.Println(pkg.PkgPath)
		fmt.Println("ignored:", pkg.IgnoredFiles)
		fmt.Println("exported:", pkg.ExportFile)
		fmt.Println("gofiles:", pkg.GoFiles)

		fset := cfg.Fset
		for _, f := range pkg.Syntax {
			type typeDef struct {
				Name string
				Kind string
			}
			var typeDefs []typeDef
			for _, decl := range f.Decls {
				decl, ok := decl.(*ast.GenDecl)
				if !ok {
					continue
				}
				if decl.Tok != token.TYPE {
					continue
				}
				for _, spec := range decl.Specs {
					spec, ok := spec.(*ast.TypeSpec)
					if !ok {
						continue
					}
					if !token.IsExported(spec.Name.Name) {
						continue
					}
					typeDefs = append(typeDefs, typeDef{
						Name: spec.Name.Name,
						Kind: fmt.Sprintf("%T", spec.Type),
					})
				}
			}

			if len(typeDefs) > 0 {
				filename := fset.File(f.Pos()).Name()
				fmt.Println(filename)
				for _, def := range typeDefs {
					fmt.Printf("\t%-20s%-20s\n", def.Name, def.Kind)
				}
			}
		}
	}
}
