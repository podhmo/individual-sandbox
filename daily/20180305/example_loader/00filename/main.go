package main

import (
	"fmt"
	"go/parser"
	"log"
	"os/user"
	"strings"

	"golang.org/x/tools/go/loader"
)

func p(path string) {
	u, _ := user.Current()
	fmt.Println(strings.Replace(path, u.HomeDir, "~", 1))
}

func main() {
	pkgname := "golang.org/x/tools/refactor/rename"

	c := loader.Config{
		ParserMode: parser.PackageClauseOnly,
	}
	c.Import(pkgname)
	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}
	pkginfo := prog.Package(pkgname)
	fset := prog.Fset

	p(fset.File(pkginfo.Files[0].Pos()).Name())
	p(fset.File(pkginfo.Files[len(pkginfo.Files)-1].Pos()).Name())
}
