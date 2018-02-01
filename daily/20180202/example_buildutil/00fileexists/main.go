package main

import (
	"bufio"
	"fmt"
	"log"

	"golang.org/x/tools/go/buildutil"
)

func main() {
	pkgs := map[string]map[string]string{
		"foo": map[string]string{
			"foo.go": `
package foo

type Foo struct{}
`,
		},
	}
	ctxt := buildutil.FakeContext(pkgs)
	{
		fmt.Printf("GOROOT is %q\n", ctxt.GOROOT)
		fmt.Printf("GOPATH is %q\n", ctxt.GOPATH)
	}

	{
		fmt.Println("readdir ?", "/go/src/foo")
		fs, err := buildutil.ReadDir(ctxt, "/go/src/foo")
		if err != nil {
			log.Fatal(err)
		}
		for _, f := range fs {
			fmt.Println("	", f)
		}

	}

	{
		fmt.Println("exists ? ", "/go/src/foo/foo.go")
		fmt.Println("	", buildutil.FileExists(ctxt, "/go/src/foo/foo.go"))
		fmt.Println("exists ? ", "/go/src/bar/bar.go")
		fmt.Println("	", buildutil.FileExists(ctxt, "/go/src/bar/bar.go"))
	}

	{
		fmt.Println("openfile", "/go/src/foo/foo.go")
		f, err := buildutil.OpenFile(ctxt, "/go/src/foo/foo.go")
		defer f.Close()
		if err != nil {
			log.Fatal(err)
		}
		sc := bufio.NewScanner(f)
		for sc.Scan() {
			fmt.Println("	", sc.Text())
		}
	}
}
