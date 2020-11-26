package astmpl

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"reflect"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"text/template"

	"golang.org/x/tools/go/ast/astutil"
)

type REPLACED struct{}

// TODO: use goimports

func FileName(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, _ := rfunc.FileLine(rfunc.Entry())
	return filename
}

type Template struct {
	Filename  string
	Variables map[string]bool
	Body      string

	*template.Template
}

var (
	m = regexp.MustCompile("__([a-zA-Z_0-9]+)__")
)

func Parse(fname string) (*Template, error) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, fname, nil, parser.ParseComments)
	if err != nil {
		return nil, fmt.Errorf("parse file %w", err)
	}
	vars := map[string]bool{}

	astutil.DeleteImport(fset, f, "m/astmpl") // FIXME

	for _, cg := range f.Comments {
		for _, c := range cg.List {
			if strings.Contains(c.Text, "// REPLACE:") {
				varname := strings.TrimSpace(strings.Replace(c.Text, "// REPLACE:", "", 1))
				// TODO: parse text/template
				vars[varname] = true
			}
		}
	}

	astutil.Apply(f, func(c *astutil.Cursor) bool {
		node := c.Node()
		switch node := node.(type) {
		case *ast.Ident:
			// __FOO__XXX => {{.FOO}}XXX
			if m.MatchString(node.Name) {
				vars[m.FindStringSubmatch(node.Name)[1]] = true
				node.Name = m.ReplaceAllString(node.Name, "{{.$1}}")
			}
		case *ast.TypeSpec:
			if m.MatchString(node.Name.Name) {
				// type __FOO__XXX asttmpl.REPLACED => type {{.FOO}}XXX {{.FOOXXX}}
				sym := m.ReplaceAllString(node.Name.Name, "$1")
				if rhs, ok := node.Type.(*ast.SelectorExpr); ok {
					if rhs.Sel.Name == "REPLACED" {
						if x, ok := rhs.X.(*ast.Ident); ok && x.Name == "astmpl" {
							node.Type = &ast.Ident{NamePos: rhs.Pos(), Name: fmt.Sprintf("{{.%s}}", sym)}
						}
					}
				}
			}
		}
		return true
	}, nil,
	)

	var buf bytes.Buffer
	p := &printer.Config{Tabwidth: 8}
	if err := p.Fprint(&buf, fset, f); err != nil {
		return nil, fmt.Errorf("print ast %w", err)
	}

	if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
		fmt.Fprintf(os.Stderr, buf.String())
	}
	tmpl, err := template.New("x").Parse(buf.String())
	if err != nil {
		return nil, fmt.Errorf("parse template %w", err)
	}
	return &Template{
		Filename:  fname,
		Body:      buf.String(),
		Variables: vars,
		Template:  tmpl,
	}, nil
}
