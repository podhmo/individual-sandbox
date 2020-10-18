package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"runtime"
	"strings"
	"text/template"

	"github.com/podhmo/reflect-openapi/pkg/shape"
	"golang.org/x/tools/go/ast/astutil"
)

func FileName(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, _ := rfunc.FileLine(rfunc.Entry())
	return filename
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Todo struct{}

func ListTodo() []Todo { return nil }

func run() error {
	fname := filepath.Join(filepath.Dir(FileName(main)), "gen.go")
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, fname, nil, parser.ParseComments)
	if err != nil {
		return fmt.Errorf("parse file %w", err)
	}

	m := regexp.MustCompile("__([a-zA-Z_0-9]+)__")
	rm := regexp.MustCompile("^__[a-zA-Z_0-9]+__$")
	astutil.Apply(f, func(c *astutil.Cursor) bool {
		node := c.Node()
		if node, ok := node.(*ast.Ident); ok {
			if m.MatchString(node.Name) {
				node.Name = m.ReplaceAllString(node.Name, "{{.$1}}")
			}
		}
		if node, ok := node.(*ast.TypeSpec); ok {
			if matched := rm.FindString(node.Name.Name); matched != "" {
				node.Type = &ast.Ident{
					NamePos: node.Type.Pos(),
					Name:    fmt.Sprintf(`{{.%sBody}}`, strings.Trim(matched, "_")),
				}
				return true
			}
		}
		return true
	}, nil,
	)

	var buf bytes.Buffer
	p := &printer.Config{Tabwidth: 8}
	if err := p.Fprint(&buf, fset, f); err != nil {
		return fmt.Errorf("print ast %w", err)
	}

	tmpl := template.Must(template.New("x").Parse(buf.String()))
	tmpl.Execute(os.Stdout, map[string]string{
		"Name": "Foo",
		"NameBody": `struct {
	Name string
}`,
	})
	return nil
}

func TypeName(s shape.Shape) string {
	switch s := s.(type) {
	case shape.Container:
		switch len(s.Args) {
		case 1:
			return "[]" + TypeName(s.Args[0])
		case 2:
			return strings.Repeat("*", s.GetLv()) + s.GetName() + "[" + TypeName(s.Args[0]) + "]" + TypeName(s.Args[1])
		default:
			panic("hmm")
		}
	default:
		return strings.Repeat("*", s.GetLv()) + strings.TrimPrefix(strings.TrimPrefix(s.GetPackage()+"."+s.GetName(), "."), "main.")
	}
}
