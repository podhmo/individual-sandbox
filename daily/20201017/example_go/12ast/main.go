package main

import (
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
	"strconv"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/shape"
	"golang.org/x/tools/go/ast/astutil"
)

func FileName(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, _ := rfunc.FileLine(rfunc.Entry())
	return filename
}
func FuncName(fn interface{}) string {
	fullname := runtime.FuncForPC(reflect.ValueOf(fn).Pointer()).Name()
	parts := strings.Split(fullname, ".")
	return parts[len(parts)-1]
}

func CollectVars(f *ast.File, m *regexp.Regexp) map[*ast.TypeSpec]string {
	vars := map[*ast.TypeSpec]string{}
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
			if !m.MatchString(spec.Name.Name) {
				continue
			}
			vars[spec] = spec.Name.Name
		}
	}
	return vars
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

	m := regexp.MustCompile("^__[a-zA-Z_0-9]+__$")
	vars := CollectVars(f, m)
	table := map[string]string{
		"__ACTION__":      FuncName(ListTodo),
		"__RETURN_TYPE__": TypeName(shape.Extract(ListTodo).(shape.Function).Returns.Values[0]),
		"__METHOD__":      strconv.Quote("GET"),
		"__PATH__":        strconv.Quote("/todo"),
	}

	// TODO: copy
	sm := regexp.MustCompile("__[a-zA-Z_0-9]+__")
	astutil.Apply(f, func(c *astutil.Cursor) bool {
		node := c.Node()
		if node, ok := node.(*ast.Ident); ok {
			if k := sm.FindString(node.Name); k != "" {
				if v, ok := table[k]; ok {
					node.Name = strings.ReplaceAll(node.Name, k, v)
				}
			}
			return false
		}
		if node, ok := node.(*ast.TypeSpec); ok {
			if _, ok := vars[node]; ok {
				c.Delete()
				return false
			}
		}
		return true
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

	p := &printer.Config{Tabwidth: 8}
	if err := p.Fprint(os.Stdout, fset, f); err != nil {
		return fmt.Errorf("print ast %w", err)
	}
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
