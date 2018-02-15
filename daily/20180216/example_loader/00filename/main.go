package main

import (
	"fmt"
	"go/ast"
	"go/printer"
	"log"
	"os"
	"path/filepath"

	"golang.org/x/tools/go/loader"
)

// transformAST
func transformAST(f *ast.File) *ast.File {
	ast.Inspect(f, func(n ast.Node) bool {
		if n, ok := n.(*ast.Ident); ok {
			n.Name = fmt.Sprintf("*%s*", n.Name)
		}
		return true
	})
	return f
}

func main() {
	conf := &loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false // skip type check
		},
	}
	conf.CreateFromFilenames(".", "main.go")
	prog, err := conf.Load()
	if err != nil {
		log.Fatal(err)
	}

	dst := "./output"

	for _, pkg := range prog.Created {
		for _, f := range pkg.Files {
			f := transformAST(f)

			// resolve filename
			filename := filepath.Base(prog.Fset.Position(f.Pos()).Filename)

			if err := os.MkdirAll(dst, 0744); err != nil {
				log.Fatal(err)
			}

			log.Printf("write %s\n", filepath.Join(dst, filename))
			wf, err := os.Create(filepath.Join(dst, filename))
			if err != nil {
				log.Fatal(err)
			}
			defer wf.Close()
			printer.Fprint(wf, prog.Fset, f)
		}
	}
}
