package main

// hmm
import (
	"fmt"
	"go/parser"
	"go/token"
	"log"
	"os"

	"github.com/podhmo/astknife/patchwork2"
	"github.com/podhmo/astknife/patchwork2/lookup"
	"github.com/podhmo/printer"
)

func main() {
	code0 := `
package p
import "fmt"
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
	fmt.Println("**", len(f2.Comments))
	for _, c := range f2.Comments {
		c.List[0].Text = fmt.Sprintf("// ** %d %q %s\n", c.Pos()-f2.Pos(), c.Text(), fset.Position(c.Pos()))
	}
	fmt.Println("file base", fset.File(f2.Pos()).Base())
	fmt.Println("file end", fset.File(f2.Pos()).Size())
	fmt.Println(f2.Decls[1].Pos(), fset.Position(f2.Decls[1].Pos()))
	printer.Fprint(os.Stdout, fset, f2)
}
