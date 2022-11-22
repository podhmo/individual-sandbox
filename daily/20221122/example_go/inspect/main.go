package main

import (
	"flag"
	"fmt"
	"os"

	"golang.org/x/tools/go/packages"
)

// https://pkg.go.dev/golang.org/x/tools/go/packages#pkg-examples

func main() {
	flag.Parse()

	// Many tools pass their command-line arguments (after any flags)
	// uninterpreted to packages.Load so that it can interpret them
	// according to the conventions of the underlying build system.
	cfg := &packages.Config{Mode: packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes}
	pkgs, err := packages.Load(cfg, flag.Args()...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "load: %v\n", err)
		os.Exit(1)
	}
	if packages.PrintErrors(pkgs) > 0 {
		os.Exit(1)
	}

	// Print the names of the source files
	// for each package listed on the command line.
	for i, pkg := range pkgs {
		fmt.Printf("name:%s\tid:%q\t	ismain:%v:\tfiles:%s\n", pkg.Name, pkg.ID, pkg.Types.Name() == "main", pkg.GoFiles)
		s := pkg.Types.Scope()
		for _, name := range s.Names() {
			fmt.Printf("\t%s %v\n", name, s.Lookup(name))
		}
		if len(pkgs)-1 > i {
			fmt.Println("")
		}
	}
}
