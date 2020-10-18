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

	m := regexp.MustCompile("__[a-zA-Z_0-9]+__")
	astutil.Apply(f, func(c *astutil.Cursor) bool {
		node := c.Node()
		if node, ok := node.(*ast.TypeSpec); ok {
			if matched := m.FindString(node.Name.Name); matched != "" {
				// fmt.Printf("%[1]T %[1]v\n", c.Parent().(*ast.GenDecl).Doc)
				doc := c.Parent().(*ast.GenDecl).Doc
				if doc != nil {
					// TODO: move
					comments := make([]*ast.CommentGroup, 0, len(f.Comments))
					for _, cg := range f.Comments {
						if doc == cg {
							cg = &ast.CommentGroup{
								List: []*ast.Comment{{
									Slash: cg.Pos(),
									Text:  "// xxx",
								}},
							}
							comments = append(comments, cg)
							continue
						}
						comments = append(comments, cg)
					}
					f.Comments = comments
				}
				node.Type = &ast.Ident{Name: `struct {
	Name string
}`}
				return false
			}
		}
		return true
	}, nil,
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
