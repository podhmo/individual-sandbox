package main

// hmm
import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"github.com/k0kubun/pp"
	"github.com/podhmo/astknife/patchwork2"
	"github.com/podhmo/astknife/patchwork2/lookup"
)

func main() {
	code0 := `
package p
import "fmt"
// F : 0
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

// F : 1
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

	fref := patchwork2.NewRef().NewFileRef(f0)
	patchwork2.Append(fref, lookup.Lookup("F", f1))

	f2 := fref.ToFile(fset, "new.go")

	for _, c := range f2.Comments {
		c.List[0].Text = fmt.Sprintf("// ** %d %q %s\n", c.Pos()-f2.Pos(), c.Text(), fset.Position(c.Pos()))
	}

	printer.Fprint(os.Stdout, fset, f2)
	fmt.Println("----------------------------------------")
	pp.Println(fset.File(f0.Pos()))
	fmt.Println("----------------------------------------")
	pp.Println(fset.File(f2.Pos()))
	fmt.Println("----------------------------------------")
	// ast.Inspect(f2, func(n ast.Node) bool {
	// 	if n != nil {
	// 		fmt.Printf("%T (%d,%d)\n", n, n.Pos()-f2.Pos(), n.End()-f2.Pos())
	// 	}
	// 	return true
	// })
	// fmt.Println("----------------------------------------")
}
