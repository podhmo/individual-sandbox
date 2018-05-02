package main

import (
	"fmt"
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/gomvpkg-light/build"
	"golang.org/x/tools/go/buildutil"
	"golang.org/x/tools/go/loader"
)

func fakeContext(pkgs map[string][]string) *build.OriginalContext {
	pkgs2 := make(map[string]map[string]string)
	for path, files := range pkgs {
		filemap := make(map[string]string)
		for i, contents := range files {
			filemap[fmt.Sprintf("%d.go", i)] = contents
		}
		pkgs2[path] = filemap
	}
	pp.Println(pkgs2)
	return buildutil.FakeContext(pkgs2)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctxt := fakeContext(map[string][]string{
		"foo": {`package foo; type T int`},
		"bar": {`package bar`},
		"main": {`package main

import "foo"

var _ foo.T
`},
	})

	c := loader.Config{Build: ctxt}
	c.Import("main")
	_, err := c.Load()
	return err
}
