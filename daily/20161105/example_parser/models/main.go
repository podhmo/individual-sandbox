package main

import (
	"go/parser"
	"go/token"
	"log"

	"flag"

	"github.com/davecgh/go-spew/spew"
)

var target *string

func init() {
	target = flag.String("target", "", "target file")
}

func main() {
    flag.Parse()
	log.Println(*target)
	if *target == "" {
		log.Fatal("app -target <file>")
	}
	run(*target)
}

func run(fname string) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, fname, nil, 0)
	if err != nil {
		log.Fatal(err)
	}
	spew.Dump(f)
}
