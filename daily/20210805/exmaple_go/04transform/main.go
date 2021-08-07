package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"regexp"
	"sort"
)

func Mark(s string, args ...interface{}) {}

const source = `package foo

// Foo: -
func Foo(){
	// print hello
	Mark("xxx")
	fmt.Println("Hello")
	// hmm
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func ExtractCallName(expr *ast.CallExpr) string {
	return extractNameFromExpr(expr.Fun)
}

func extractNameFromExpr(fun ast.Expr) string {
	switch fun := fun.(type) {
	case *ast.Ident:
		return fun.Name
	case *ast.SelectorExpr:
		return extractNameFromExpr(fun.X) + "." + fun.Sel.Name
	default:
		panic(fmt.Sprintf("%T %+v", fun, fun))
	}
}

func run() error {
	fset := token.NewFileSet()
	// f, err := parser.ParseExprFrom(fset, "p", source, parser.ParseComments|parser.AllErrors)
	f, err := parser.ParseFile(fset, "p", source, parser.ParseComments|parser.AllErrors)
	if err != nil {
		return fmt.Errorf("parse %w", err)
	}

	// ast.Fprint(os.Stdout, fset, f, nil)
	ast.Inspect(f, func(node ast.Node) (cont bool) {
		switch t := node.(type) {
		case *ast.CallExpr:
			name := ExtractCallName(t)
			if name == "Mark" {
				f.Comments = append(f.Comments, &ast.CommentGroup{List: []*ast.Comment{{Slash: t.Pos() - 1, Text: "/* XXX "}}})
				f.Comments = append(f.Comments, &ast.CommentGroup{List: []*ast.Comment{{Slash: t.End() + 1, Text: "XXX */"}}})
			}
			return false
		}
		return true
	})

	// 適切な位置に挿入する必要がある。
	sort.Slice(f.Comments, func(i, j int) bool { return f.Comments[i].Pos() < f.Comments[j].Pos() })

	cfg := &printer.Config{Tabwidth: 8, Mode: printer.TabIndent}
	var b bytes.Buffer
	cfg.Fprint(&b, fset, f)

	re := regexp.MustCompile(`(?s)/\* XXX.*XXX \*/`)
	buf := re.ReplaceAll(b.Bytes(), []byte("doSomething(x,y,z)\n"))
	output, err := format.Source(buf)
	if err != nil {
		return err
	}
	fmt.Println(string(output))
	return nil
}
