package main

import (
	"fmt"
	"go/types"
	"log"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package main

import "fmt"
import "os"

func main(){
	fmt.Println("hello world")
	os.Exit(0)
}
`
	c := loader.Config{}
	f, err := c.ParseFile("main.go", source)
	if err != nil {
		return err
	}
	c.CreateFromFiles("main", f)

	prog, err := c.Load()
	if err != nil {
		return err
	}

	info := prog.Package("main")
	for ident, ob := range info.Uses {
		fmt.Printf("ident=%+v, ob=%+v\n", ident, ob)
		fmtpkg := prog.Package("fmt").Pkg
		if fn, ok := ob.(*types.Func); ok {
			fmt.Println("	id", fn.Id())
			fmt.Println("	name", fn.Name())
			fmt.Println("	scope", fn.Scope())
			fmt.Println("	parent", fn.Parent() == fmtpkg.Scope())
		}
	}
	return nil
}
