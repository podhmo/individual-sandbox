package clientgen

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"path/filepath"
	"regexp"
	"text/template"

	"golang.org/x/tools/go/ast/astutil"
)

type Vars struct {
	Types  map[*ast.TypeSpec]string
	Values map[*ast.ValueSpec]string
}

func CollectVars(f *ast.File, m *regexp.Regexp) Vars {
	vars := Vars{
		Types:  map[*ast.TypeSpec]string{},
		Values: map[*ast.ValueSpec]string{},
	}
	for _, decl := range f.Decls {
		decl, ok := decl.(*ast.GenDecl)
		if !ok {
			continue
		}
		switch decl.Tok {
		case token.TYPE:
			for _, spec := range decl.Specs {
				spec, ok := spec.(*ast.TypeSpec)
				if !ok {
					continue
				}
				if !m.MatchString(spec.Name.Name) {
					continue
				}
				vars.Types[spec] = spec.Name.Name
			}
		case token.VAR:
			allok := true
			for _, spec := range decl.Specs {
				spec, ok := spec.(*ast.ValueSpec)
				if !ok {
					continue
				}
				for _, ident := range spec.Names {
					if !m.MatchString(ident.Name) {
						allok = false
						break
					}
				}
				if allok {
					vars.Values[spec] = spec.Names[0].Name // xxx
				}
			}
		default:
			continue
		}
	}
	return vars
}

func BuildTemplate(fset *token.FileSet, fname string, f *ast.File) (*template.Template, error) {
	rm := regexp.MustCompile("^__[a-zA-Z_0-9]+__$")
	m := regexp.MustCompile("__([a-zA-Z_0-9]+)__")

	vars := CollectVars(f, rm)
	astutil.Apply(f,
		// pre
		func(c *astutil.Cursor) bool {
			node := c.Node()
			if node, ok := node.(*ast.Ident); ok {
				if m.MatchString(node.Name) {
					node.Name = m.ReplaceAllString(node.Name, "{{.$1}}")
				}
			}

			switch node := node.(type) {
			case *ast.TypeSpec:
				if _, ok := vars.Types[node]; ok {
					c.Delete()
					return false
				}
			case *ast.ValueSpec:
				if _, ok := vars.Values[node]; ok {
					c.Delete()
					return false
				}
			default:
				return true
			}
			return true // never
		},
		// post
		func(c *astutil.Cursor) bool {
			node := c.Node()
			if node, ok := node.(*ast.GenDecl); ok {
				if len(node.Specs) == 0 {
					c.Delete()
					return true
				}
			}
			return true
		},
	)

	var buf bytes.Buffer
	p := &printer.Config{Tabwidth: 8}
	if err := p.Fprint(&buf, fset, f); err != nil {
		return nil, fmt.Errorf("print ast %w", err)
	}

	tmpl := template.New(filepath.Base(fname))
	tmpl, err := tmpl.Parse(buf.String())
	if err != nil {
		return nil, fmt.Errorf("parse template %w", err)
	}
	return tmpl, err
}
