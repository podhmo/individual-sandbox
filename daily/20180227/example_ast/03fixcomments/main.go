package main

// hmm
import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"io"
	"log"
	"os"

	"github.com/podhmo/astknife/action/replace"
	"github.com/sergi/go-diff/diffmatchpatch"
)

func main() {
	code0 := `
package p
import "fmt"
// F :
func F() int {
	return 10
}

// G :
func G() int {
	return 20
}
`
	code1 := `
package p

// F :
func F() int {
	// xxx
	x := 5
	return x + x
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0.go", code0, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	f1, err := parser.ParseFile(fset, "f1.go", code1, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	f0F := f0.Scope.Lookup("F")
	f1F := f1.Scope.Lookup("F")

	if _, err := replace.ToplevelToFile(f0, f0.Scope.Lookup("F"), f1.Scope.Lookup("F")); err != nil {
		log.Fatal(err)
	}
	printer.Fprint(os.Stdout, fset, f0)

	{
		var b bytes.Buffer
		dumpPositions(f0, &b)

		move(f1F.Decl.(ast.Node), int((f0F.Pos()-f0.Pos())-(f1F.Pos()-f1.Pos())))

		var b2 bytes.Buffer
		dumpPositions(f0, &b2)

		dmp := diffmatchpatch.New()
		diffs := dmp.DiffMain(b.String(), b2.String(), false)
		fmt.Println(dmp.DiffPrettyText(diffs))
	}
	printer.Fprint(os.Stdout, fset, f0)
}

func dumpPositions(f ast.Node, w io.Writer) {
	if f == nil {
		return
	}
	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
			fmt.Fprintf(w, "%T (%d)\n", node, node.Pos())
		}
		return true
	})
}

func move(f ast.Node, offset int) {
	if f == nil {
		return
	}
	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
			switch x := node.(type) {
			case *ast.Comment:
				x.Slash = token.Pos(int(x.Slash) + offset)
			case *ast.Field:
				// if len(x.Names) > 0 {
				//     return x.Names[0].Pos()
				// }
				// return x.Type.Pos()
			case *ast.FieldList:
				if x.Opening.IsValid() {
					// return x.Opening
					x.Opening = token.Pos(int(x.Opening) + offset)
				}
				// // the list should not be empty in this case;
				// // be conservative and guard against bad ASTs
				// if len(f.List) > 0 {
				// 	return f.List[0].Pos()
				// }
				// return token.NoPos
			case *ast.BadExpr:
				x.From = token.Pos(int(x.From) + offset)
			case *ast.Ident:
				x.NamePos = token.Pos(int(x.NamePos) + offset)
			case *ast.Ellipsis:
				x.Ellipsis = token.Pos(int(x.Ellipsis) + offset)
			case *ast.BasicLit:
				x.ValuePos = token.Pos(int(x.ValuePos) + offset)
			case *ast.FuncLit:
				// x.Type.Pos()
			case *ast.CompositeLit:
				if x.Type != nil {
					// return x.Type.Pos()
				} else {
					x.Lbrace = token.Pos(int(x.Lbrace) + offset)
				}
			case *ast.ParenExpr:
				x.Lparen = token.Pos(int(x.Lparen) + offset)
			case *ast.SelectorExpr:
				// x.X.Pos()
			case *ast.IndexExpr:
				// x.X.Pos()
			case *ast.SliceExpr:
				// x.X.Pos()
			case *ast.TypeAssertExpr:
				// x.X.Pos()
			case *ast.CallExpr:
				// x.Fun.Pos()
			case *ast.StarExpr:
				x.Star = token.Pos(int(x.Star) + offset)
			case *ast.UnaryExpr:
				x.OpPos = token.Pos(int(x.OpPos) + offset)
			case *ast.BinaryExpr:
				// x.X.Pos()
			case *ast.KeyValueExpr:
				// x.Key.Pos()
			case *ast.ArrayType:
				x.Lbrack = token.Pos(int(x.Lbrack) + offset)
			case *ast.StructType:
				x.Struct = token.Pos(int(x.Struct) + offset)
			case *ast.FuncType:
				if x.Func.IsValid() || x.Params == nil { // see issue 3870
					x.Func = token.Pos(int(x.Func) + offset)
				}
				// return x.Params.Pos() // interface method declarations have no "func" keyword
			case *ast.InterfaceType:
				x.Interface = token.Pos(int(x.Interface) + offset)
			case *ast.MapType:
				x.Map = token.Pos(int(x.Map) + offset)
			case *ast.ChanType:
				x.Begin = token.Pos(int(x.Begin) + offset)
			case *ast.BadStmt:
				x.From = token.Pos(int(x.From) + offset)
			case *ast.DeclStmt:
				// x.Decl.Pos()
			case *ast.EmptyStmt:
				x.Semicolon = token.Pos(int(x.Semicolon) + offset)
			case *ast.LabeledStmt:
				// x.Label.Pos()
			case *ast.ExprStmt:
				// x.X.Pos()
			case *ast.SendStmt:
				// x.Chan.Pos()
			case *ast.IncDecStmt:
				// x.X.Pos()
			case *ast.AssignStmt:
				// x.Lhs[0].Pos()
			case *ast.GoStmt:
				x.Go = token.Pos(int(x.Go) + offset)
			case *ast.DeferStmt:
				x.Defer = token.Pos(int(x.Defer) + offset)
			case *ast.ReturnStmt:
				x.Return = token.Pos(int(x.Return) + offset)
			case *ast.BranchStmt:
				x.TokPos = token.Pos(int(x.TokPos) + offset)
			case *ast.BlockStmt:
				x.Lbrace = token.Pos(int(x.Lbrace) + offset)
			case *ast.IfStmt:
				x.If = token.Pos(int(x.If) + offset)
			case *ast.CaseClause:
				x.Case = token.Pos(int(x.Case) + offset)
			case *ast.SwitchStmt:
				x.Switch = token.Pos(int(x.Switch) + offset)
			case *ast.TypeSwitchStmt:
				x.Switch = token.Pos(int(x.Switch) + offset)
			case *ast.CommClause:
				x.Case = token.Pos(int(x.Case) + offset)
			case *ast.SelectStmt:
				x.Select = token.Pos(int(x.Select) + offset)
			case *ast.ForStmt:
				x.For = token.Pos(int(x.For) + offset)
			case *ast.RangeStmt:
				x.For = token.Pos(int(x.For) + offset)
			case *ast.ImportSpec:
				// if s.Name != nil {
				//     return s.Name.Pos()
				// }
				// return s.Path.Pos()
			case *ast.ValueSpec:
				// return s.Names[0].Pos()
			case *ast.TypeSpec:
				// return s.Name.Pos() }
			case *ast.BadDecl:
				x.From = token.Pos(int(x.From) + offset)
			case *ast.GenDecl:
				x.TokPos = token.Pos(int(x.TokPos) + offset)
			case *ast.FuncDecl:
				// d.Type.Pos()
			case *ast.File:
				x.Package = token.Pos(int(x.Package) + offset)
			}
		}
		return true
	})
}
