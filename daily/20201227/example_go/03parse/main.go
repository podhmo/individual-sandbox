package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

func main() {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments)
	if err != nil {
		panic(err)
	}
	for _, decl := range f.Decls {
		decl, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue

		}
		// if decl.Name.Name != "GetAlert" {
		// 	continue
		// }
		if strings.ToLower(decl.Name.Name)[0] == decl.Name.Name[0] {
			continue
		}

		fmt.Printf("%s:\n", decl.Name.Name)
		ast.Inspect(decl, func(node ast.Node) bool {
			switch node := node.(type) {
			case *ast.CallExpr:
				switch fun := node.Fun.(type) {
				case *ast.SelectorExpr:
					switch x := fun.X.(type) {
					case *ast.Ident:
						if x.Name != "c" {
							return true
						}
						if fun.Sel.Name == "JsonApiErr" {
							return true
						}
						if len(node.Args) != 1 {
							fmt.Fprintf(os.Stderr, "  - %s.%s() -- %+#v\n", x.Name, fun.Sel.Name, node.Args[0])
							return true
						}
						switch arg := node.Args[0].(type) {
						case *ast.BasicLit:
							fmt.Printf("  %s: %s\n", arg.Value, fun.Sel.Name)
						default:
							fmt.Fprintf(os.Stderr, "  - %s.%s() -- ?%v?\n", x.Name, fun.Sel.Name, arg)
						}
					default:
						fmt.Fprintf(os.Stderr, "    -- ??fun.x %+#v\n", fun)
					}
				case *ast.Ident:
					// ident := fun
					// fmt.Fprintf(os.Stderr, "  -- ?ident %s\n", ident.Name)
				default:
					fmt.Fprintf(os.Stderr, "  -- ?? %T\n", fun)
				}
			}
			return true
		})
	}
}
