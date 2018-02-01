package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"

	"golang.org/x/tools/go/buildutil"
	"golang.org/x/tools/refactor/rename"
)

func main() {
	pkgs := map[string]map[string]string{
		"model": {
			"person.go": `
package model
type Person struct {
	Name string
	Age int
}

func (p *Person)String() string {
	return p.Name
}
`,
		},
	}
	ctxt := buildutil.FakeContext(pkgs)
	fmt.Println(buildutil.FileExists(ctxt, "/go/src/model/person.go"))

	// this is fake implementation of rename.writeFile
	writeFile = func(filename string, content []byte) error {
		got[filepath.ToSlash(filename)] = string(content)
		return nil
	}

	if err := rename.Main(ctxt, "", `"model".Person`, `person`); err != nil {
		log.Fatal(err)
	}

	f, err := buildutil.OpenFile(ctxt, "/go/src/model/person.go")
	if err != nil {
		log.Fatal(err)
	}
	io.Copy(os.Stdout, f)
}
