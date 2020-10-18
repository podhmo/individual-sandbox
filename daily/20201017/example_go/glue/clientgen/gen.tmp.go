package clientgen

// import (
// 	"fmt"
// 	"go/ast"
// 	"go/parser"
// 	"go/printer"
// 	"go/token"
// 	"io"
// 	"m/glue"
// 	"os"
// 	"path/filepath"
// )

// type Config struct {
// 	GetOutput func(name string) io.Writer
// 	Driver    string
// 	Decoder   string
// }

// func (c *Config) Gen(g *glue.Glue) error {
// 	if c.GetOutput == nil {
// 		c.GetOutput = func(name string) io.Writer {
// 			return os.Stdout
// 		}
// 	}
// 	if c.Decoder == "" {
// 		c.Decoder = "decoder.go"
// 	}
// 	if c.Driver == "" {
// 		c.Driver = "driver.go"
// 	}

// 	filename := FileName(c.Gen)
// 	dir := filepath.Dir(filename)

// 	fset := token.NewFileSet()
// 	pkgs, err := parser.ParseDir(fset, dir, nil, parser.ParseComments)
// 	if err != nil {
// 		return fmt.Errorf("parse dir %w", err)
// 	}
// 	pkg := pkgs["clientgen"]

// 	p := &printer.Config{
// 		Tabwidth: 8,
// 	}

// 	emits := []func(){}
// 	var imports []*ast.ImportSpec
// 	{
// 		w := c.GetOutput(c.Driver)
// 		f := pkg.Files[filepath.Join(dir, c.Driver)]
// 		imports = append(imports, f.Imports...)
// 		decls := make([]ast.Decl, 0, len(f.Decls))
// 		for _, decl := range f.Decls {
// 			if decl, ok := decl.(*ast.GenDecl); ok && decl.Tok == token.IMPORT {
// 				continue
// 			}
// 			decls = append(decls, decl)
// 		}
// 		f.Decls = decls
// 		emits = append(emits, func() {
// 			fmt.Fprint(w, "// ")
// 			p.Fprint(w, fset, f)
// 			fmt.Fprintln(w, "\n")
// 			fmt.Fprintln(w, "")
// 		})
// 	}
// 	{
// 		w := c.GetOutput(c.Decoder)
// 		f := pkg.Files[filepath.Join(dir, c.Decoder)]
// 		imports = append(imports, f.Imports...)
// 		decls := make([]ast.Decl, 0, len(f.Decls))
// 		for _, decl := range f.Decls {
// 			if decl, ok := decl.(*ast.GenDecl); ok && decl.Tok == token.IMPORT {
// 				continue
// 			}
// 			decls = append(decls, decl)
// 		}
// 		f.Decls = decls
// 		emits = append(emits, func() {
// 			fmt.Fprint(w, "// ")
// 			p.Fprint(w, fset, f)
// 			fmt.Fprintln(w, "")
// 			fmt.Fprintln(w, "")
// 		})
// 	}

// 	specs := make([]ast.Spec, len(imports))
// 	for i := range imports {
// 		specs[i] = imports[i]
// 	}
// 	w := c.GetOutput("main.go")
// 	fmt.Fprintln(w, "package", g.Name())
// 	fmt.Fprintln(w, "")
// 	p.Fprint(w, fset, []ast.Decl{
// 		&ast.GenDecl{
// 			Tok:   token.IMPORT,
// 			Specs: specs,
// 		},
// 	})
// 	fmt.Fprintln(w, "")
// 	fmt.Fprintln(w, "")

// 	for _, emit := range emits {
// 		emit()
// 	}
// 	return nil
// }
