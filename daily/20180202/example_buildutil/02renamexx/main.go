package main

import (
	"go/build"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"golang.org/x/tools/refactor/rename"
)

func run() error {
	root, err := ioutil.TempDir(".", "xxx")
	defer os.RemoveAll(root)

	if err != nil {
		return err
	}

	ctxt := build.Default // copied
	ctxt.GOPATH = root

	pkgdir := filepath.Join(ctxt.GOPATH, "src/github.com/podhmo/sandbox")
	if err := os.MkdirAll(pkgdir, 0755); err != nil {
		return err
	}
	code := `
package sandbox

type Person struct {
	Name string
	Age int
}

func (p *Person)String() string {
	return p.Name
}
`
	if err := ioutil.WriteFile(filepath.Join(pkgdir, "person.go"), []byte(code), 0744); err != nil {
		return err
	}

	if err := rename.Main(&ctxt, "", `"github.com/podhmo/sandbox".Person`, `person`); err != nil {
		return err
	}

	f, err := os.Open(filepath.Join(pkgdir, "person.go"))
	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, f); err != nil {
		return err
	}

	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
