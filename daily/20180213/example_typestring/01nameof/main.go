package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"log"
	"os"

	"golang.org/x/tools/go/buildutil"
	"golang.org/x/tools/go/loader"
)

func main() {
	pkgs := map[string]map[string]string{
		"github.com/podhmo/sandbox/model": {
			"person.go": `
package model

import (
  "time"
  f "fmt"
  "github.com/podhmo/sandbox/primitive"
)

type Person struct {
	Name string
	Birth time.Time
}
`,
		},
		"fmt": {"fmt.go": "package fmt"},
		"github.com/podhmo/sandbox/primitive": {"primitive.go": "package primitive"},
		"time": {
			"time.go": `
package time

type Time string
`,
		},
	}
	conf := &loader.Config{
		Build:       buildutil.FakeContext(pkgs),
		ParserMode:  parser.ParseComments,
		AllowErrors: true, // xxx
	}

	conf.Import("github.com/podhmo/sandbox/model")
	prog, err := conf.Load()

	if err != nil {
		log.Fatal(err)
	}

	extractImport(prog, prog.Package("github.com/podhmo/sandbox/model").Files[0])
}

func extractImport(prog *loader.Program, f *ast.File) {
	fmt.Println("----------------------------------------")
	for _, is := range f.Imports {
		if is.Name == nil {
			fmt.Println("*0", is.Path.Value, "as", prog.Package(is.Path.Value[1:len(is.Path.Value)-1]).Pkg.Name())
		} else {
			fmt.Println("*1", is.Path.Value, "as", is.Name.String())
		}
	}
	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, prog.Fset, f)
}
