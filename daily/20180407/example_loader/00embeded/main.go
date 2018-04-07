package main

import (
	"fmt"
	"go/ast"
	"log"
	"strings"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := strings.Join([]string{
		"package p",
		"",
		`import "bytes"`,
		"",
		"type s struct {",
		"	bytes.Buffer `json:\"-\"`",
		"	subBuffer bytes.Buffer `json:\"-\"`",
		"}",
	}, "\n")

	c := loader.Config{}
	f, err := c.ParseFile("p.go", source)
	if err != nil {
		return err
	}

	c.CreateFromFiles("p", f)
	prog, err := c.Load()
	if err != nil {
		return err
	}

	info := prog.Package("p")
	ast.Inspect(info.Files[0], func(node ast.Node) bool {
		if t, _ := node.(*ast.SelectorExpr); t != nil {
			fmt.Println(t.X, t.Sel, info.ObjectOf(t.Sel).Pkg(), info.Pkg)
			return false
		}
		return true
	})
	return nil
}
