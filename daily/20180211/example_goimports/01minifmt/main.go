package main

import (
	"bytes"
	"fmt"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	filename := "./user.go"
	input := `
package p
type User struct {Name string; Age int;}
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, filename, input, parser.ParseComments)
	if err != nil {
		return err
	}

	var buf bytes.Buffer
	c := &printer.Config{Mode: printer.TabIndent, Tabwidth: 8}
	if err := c.Fprint(&buf, fset, file); err != nil {
		return err
	}

	output, err := format.Source(buf.Bytes())
	if err != nil {
		return err
	}

	fmt.Println(string(output))
	return nil
}
