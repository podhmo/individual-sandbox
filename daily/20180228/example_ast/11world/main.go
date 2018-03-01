package main

import (
	"fmt"
	"go/parser"
	"go/token"
	"log"
	"os"

	"github.com/podhmo/astknife/patchwork3"
	"github.com/podhmo/astknife/patchwork3/lookup"
)

func main() {
	source := `
package p
// F :
func F() int {
	// internal
	return 1 + 1
} // xxx
`

	source2 := `
package p
// G :
func G() int {
	// internal
	return 10 + 10
} // xxx
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	g, err := parser.ParseFile(fset, "f1.go", source2, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	p := patchwork3.New(fset)

	fmt.Println("----------------------------------------")
	area, err := p.NewArea(f).Append(lookup.Lookup("F", f))
	area, err = area.Append(lookup.Lookup("G", g))
	if err := area.Display(os.Stdout); err != nil {
		log.Fatal(err)
	}
	fmt.Println("----------------------------------------")
}
