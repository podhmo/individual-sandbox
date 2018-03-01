package main

import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"github.com/podhmo/astknife/patchwork2/mirror"
)

func main() {
	source := `
package p

// fmt
import (
	"fmt" // hai
)

// toplevel before

// Hello : 0
func Hello(x int) string {
	if x % 2 == 0 {
		// even
		return "hello"
	}
	return "HELLO"
}	// hmm

// toplevel after
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	f2 := mirror.File(f, 1, false)
	printer.Fprint(os.Stdout, fset, f)
	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, fset, f2)
}
