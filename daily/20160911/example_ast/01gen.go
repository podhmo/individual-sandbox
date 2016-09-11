package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

type Ast struct {
	Filename string
	Source   string
	Fset     *token.FileSet
	Node     ast.Node
}

func parse(filename string, source string) (*Ast, error) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, filename, source, 0)
	if err != nil {
		return nil, err
	}
	a := Ast{Filename: filename, Source: source, Node: f, Fset: fset}
	return &a, nil
}

func modify(a *Ast, structName string) {
	ast.Inspect(a.Node, func(node ast.Node) bool {
		switch aType := node.(type) {
		case *ast.Ident:
			if aType.Name == "U" {
				aType.Name = strings.Replace(aType.Name, "U", structName, 1)
			}
		}
		// log.Printf("%[1]T: %#+[1]v\n", node)
		return true
	})
}

func main() {
	templateName := "./greeting.go"
	b, err := ioutil.ReadFile(templateName)
	if err != nil {
		log.Fatal(err)
	}
	source := string(b)
	a, err := parse(templateName, source)

	modify(a, "T") // U -> T

	log.Println("before:")
	fmt.Println(source)

	log.Println("after:")
	format.Node(os.Stdout, a.Fset, a.Node)
	if err != nil {
		log.Fatal(err)
	}
}
