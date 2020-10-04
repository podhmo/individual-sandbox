package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
)

// F : this is F
func F() {
}

func main() {
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}
func run() error {
	filename := os.Getenv("FILENAME")
	if filename == "" {
		filename = "main.go"
	}

	fset := token.NewFileSet()
	f, err := os.Open(filename)
	if err != nil {
		return err
	}

	mode := parser.ParseComments
	astf, err := parser.ParseFile(fset, filename, f, mode)
	if err != nil {
		return err
	}
	return ast.Print(fset, astf)
}
