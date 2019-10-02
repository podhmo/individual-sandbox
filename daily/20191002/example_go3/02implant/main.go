package main

import (
	"bufio"
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"reflect"
	"sort"
)

func main() {
	fmt.Println(run())
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f.go", code, parser.ParseComments)
	if err != nil {
		return err
	}
	f2, err := parser.ParseFile(fset, "f2.go", code2, parser.ParseComments)
	if err != nil {
		return err
	}

	printer.Fprint(os.Stdout, fset, f.Decls[0])
	fmt.Println("\n----------------------------------------")
	printer.Fprint(os.Stdout, fset, f2.Decls[0])
	fmt.Println("\n----------------------------------------")

	// <comment>
	// <type> <body/>
	// </body>

	{
		fdecl := f.Decls[0].(*ast.FuncDecl)
		fdecl2 := f2.Decls[0].(*ast.FuncDecl)

		pos := fdecl.Pos()
		pos2 := fdecl2.Pos()
		fdecl.Type = f2.Decls[0].(*ast.FuncDecl).Type

		zp := token.Pos(0)
		rtype := reflect.TypeOf(zp)

		ast.Inspect(fdecl.Type, func(n ast.Node) bool {
			rn := reflect.ValueOf(n)
			if rn.Kind() == reflect.Ptr {
				if rn.IsNil() {
					return true
				}
				rn = rn.Elem()
			}
			if rn.Kind() != reflect.Struct {
				return true
			}
			for i := 0; i < rn.NumField(); i++ {
				rf := rn.Field(i)
				if rf.Type() == rtype && rf.Int() > 0 {
					rf.SetInt(rf.Int() + int64(pos-pos2))
				}
			}
			return true
		})
		rlines := reflect.ValueOf(fset.File(f.Pos())).Elem().FieldByName("lines")
		lines := make([]int, 0, rlines.Len())
		for i := 0; i < rlines.Len(); i++ {
			lineno := token.Pos(rlines.Index(i).Int())
			if !(fdecl.Body.Pos() <= lineno && lineno <= fdecl.Body.End()) {
				lines = append(lines, int(lineno))
			}
		}
		fset.File(f.Pos()).SetLines(lines)

		fdecl.Body = nil

		var buf bytes.Buffer
		printer.Fprint(&buf, fset, fdecl2.Body)

		end := fdecl.Type.End()
		var clines []*ast.Comment
		s := bufio.NewScanner(&buf)
		for s.Scan() {
			clines = append(clines, &ast.Comment{
				Slash: end,
				Text:  fmt.Sprintf("//==%s", s.Text()),
			})
		}
		f.Comments = append(f.Comments, &ast.CommentGroup{
			List: clines,
		})
		sort.Slice(f.Comments, func(i, j int) bool {
			return f.Comments[i].Pos() < f.Comments[j].Pos()
		})

		printer.Fprint(os.Stdout, fset, f)
	}
	return nil
}

const code string = `
package main

// F :
// - xxx
func F(n int) string {
	// n == 0 F
	if n == 0 {
		// return F
		return "<zero>"
	}
	return fmt.Sprintf("<N%d>", n)
} // end F
`
const code2 string = `
package main

func F(n int, m int) string {
	// n == 0 F2
	if n == 0 {
		// return F2
		return "<zero>"
	}
	return fmt.Sprintf("<N%d>", n)
} // end F2
`
