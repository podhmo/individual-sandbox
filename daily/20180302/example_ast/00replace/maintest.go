package t

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	"github.com/podhmo/astknife/patchwork2"
	"github.com/podhmo/astknife/patchwork2/lookup"
	"github.com/podhmo/printer"
)

func TestReplace(t *testing.T) {
	code0 := `
	package p

	// S : 0
	type S struct {
		// Name :
		Name string // name
		// Age :
		Age string // age
		// Nickname :
		Nickname string // nickname
	}

	// F : 0
	func F() int {
		// this is f0's comment
		return 10
	}
	`
	code1 := `
package p

// F : 1
func F() int {
	// this is f1's comment
	x := 5
	return x + x
}

// G : 1
func G() int {
	// this is g1's comment
	x := 10
	return x + x
}
`
	code2 := `
package p

// hmm

// G : 2
func G() int {
	// this is g2's comment
	x := 5
	return x + x + x + x
}
`
	// todo: doc comment is not found
	type C struct {
		msg  string
		code string
		name string
	}

	candidates := []C{
		// {
		// 	msg:  "replace f0.F to f0.F",
		// 	code: code0,
		// 	name: "F",
		// },
		{
			msg:  "replace f0.F to f1.F",
			code: code1,
			name: "F",
		},
		// {
		// 	msg:  "replace f0.G to f1.G",
		// 	code: code2,
		// 	name: "G",
		// },
	}
	_ = code1
	_ = code2
	for _, c := range candidates {
		c := c
		t.Run(c.msg, func(t *testing.T) {
			fset := token.NewFileSet()
			f0, err := parser.ParseFile(fset, "f0.go", code0, parser.ParseComments)
			if err != nil {
				t.Fatal(err)
			}
			f1, err := parser.ParseFile(fset, "f1.go", c.code, parser.ParseComments)
			if err != nil {
				t.Fatal(err)
			}
			ref := patchwork2.NewRef(f0, f1)
			if err := patchwork2.Replace(ref, lookup.Lookup(c.name, f0), lookup.Lookup(c.name, f1)); err != nil {
				t.Fatal(err)
			}

			var b bytes.Buffer
			got := ref.Files[0].ToFile(fset, "newf0.go")
			printer.Fprint(&b, fset, got)
			fmt.Println(f0.Pos(), f0.End(), "actual", f0.End()-f0.Pos(), fset.File(f0.Pos()).Base(), "expected", fset.File(f0.Pos()).Size())
			fmt.Println(got.Pos(), got.End(), "actual", got.End()-got.Pos(), fset.File(got.Pos()).Base(), "expected", fset.File(got.Pos()).Size())
			ast.Inspect(f0, func(node ast.Node) bool {
				if node != nil {
					fmt.Printf("%T %v-%v s %v @ %v-%v\n", node, node.Pos(), node.End(), node.End()-node.Pos(), node.Pos()-f0.Pos(), node.End()-f0.Pos())
				}
				return true
			})
			fmt.Println("----------------------------------------")
			ast.Inspect(got, func(node ast.Node) bool {
				if node != nil {
					fmt.Printf("%T %v-%v s %v @ %v-%v\n", node, node.Pos(), node.End(), node.End()-node.Pos(), node.Pos()-got.Pos(), node.End()-got.Pos())
				}
				return true
			})
			// ast.Fprint(os.Stdout, fset, got, nil)
			t.Log(b.String())
		})
	}
}
