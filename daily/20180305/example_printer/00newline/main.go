package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"reflect"

	"github.com/podhmo/printer"
)

func main() {
	fset := token.NewFileSet()
	source := `package p

type S struct {}

// F :
func F(){
	return 1 + 1
}

// ok
`
	f, _ := parser.ParseFile(fset, "", source, parser.ParseComments)

	tf := fset.File(f.Pos())
	fmt.Println(reflect.ValueOf(tf).Elem().FieldByName("lines"))
	tf.SetLines([]int{0, 10, 11, 28, 29})
	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
            fmt.Printf("%02d: %T (%d)\n", tf.Line(node.Pos()), node, int(node.Pos())-tf.Base())
		}
		return true
	})
	printer.Fprint(os.Stdout, fset, f)
}
