package main

import (
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
)

func FileName(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, _ := rfunc.FileLine(rfunc.Entry())
	return filename
}

func main() {
	fname := filepath.Join(filepath.Dir(FileName(main)), "./templates/gen.go")
	fset := token.NewFileSet()
	code, _ := ioutil.ReadFile(fname)
	f, _ := parser.ParseFile(fset, fname, code, parser.ParseComments)

	comments := make([]*ast.CommentGroup, 0, len(f.Comments))
	for _, cg := range f.Comments {
		ok := true
		for _, c := range cg.List {
			if strings.Contains(c.Text, "Block:") {
				ok = false
			}
		}
		if ok {
			comments = append(comments, cg)
		} else {
			comments = append(comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: cg.Pos(),
						Text:  "****REPLACED****!!",
					},
				},
			})
		}
	}
	f.Comments = comments
	printer.Fprint(os.Stdout, fset, f)
}
