package main

import (
	"log"

	"github.com/k0kubun/pp"
	"golang.org/x/tools/go/loader"
)

func main() {
	source := `
package p

type I interface {
	F(s string) string
	G(s string) string
}
`
	c := loader.Config{}
	f, err := c.ParseFile("f.go", source)
	if err != nil {
		log.Fatal(err)
	}
	c.CreateFromFiles("p", f)
	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	pp.ColoringEnabled = false
	pp.Println(prog.Package("p").Pkg)
}
