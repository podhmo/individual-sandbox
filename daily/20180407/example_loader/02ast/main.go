package main

import (
	"go/ast"
	"log"
	"os"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package main

import (
	"bytes"
	"os"
)

type s struct {
	bytes.Buffer
	subBuffer bytes.Buffer
}

func (s s) NewBufferString() {}

func main(){
	bytes.NewBufferString("hello world")

	bytes := s{}
	bytes.NewBufferString()

	os.Exit(0)
}
`

	c := loader.Config{}
	f, err := c.ParseFile("main.go", source)
	if err != nil {
		return err
	}
	c.CreateFromFiles("main", f)

	prog, err := c.Load()
	if err != nil {
		return err
	}

	info := prog.Package("main")
	for _, f := range info.Files {
		ast.Fprint(os.Stdout, prog.Fset, f, nil)
	}
	return nil
}
