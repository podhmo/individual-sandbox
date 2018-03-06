package main

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"io"
	"log"
	"os"

	"github.com/k0kubun/pp"
	"golang.org/x/tools/go/loader"
)

func main() {
	c := loader.Config{
		ParserMode: parser.ParseComments,
	}
	c.Import(".")
	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}
	pkginfo := prog.Package(".")

	var b bytes.Buffer
	encoder := gob.NewEncoder(io.MultiWriter(&b, os.Stdout))
	encoder.Encode(*pkginfo.Files[0])

	var file ast.File
	decoder := gob.NewDecoder(&b)
	fmt.Println("\n----------------------------------------")
	if err := decoder.Decode(&file); err != nil {
		log.Fatal(err)
	}
	printer.Fprint(os.Stdout, prog.Fset, file)
	pp.Println(file)
}
