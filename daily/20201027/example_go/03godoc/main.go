package main

import (
	"fmt"
	"log"
	"os"

	"go/doc"
	"go/token"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(pattern []string) error {
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, pattern...)
	if err != nil {
		return fmt.Errorf("packages, load %w", err)
	}
	if len(pkgs) != 1 {
		return fmt.Errorf("unexpected packages count %d", len(pkgs))
	}

	pkg := pkgs[0]
	dpkg, err := doc.NewFromFiles(fset, pkg.Syntax, pkg.PkgPath) // , doc.AllDecls | doc.AllMethods)
	if err != nil {
		return fmt.Errorf("doc, new from files %w", err)
	}

	fmt.Println("Doc", dpkg.Doc)
	fmt.Println("Name", dpkg.Name)
	fmt.Println("ImportPath", dpkg.ImportPath)
	fmt.Println("Imports", dpkg.Imports)
	fmt.Println("Filenames", dpkg.Filenames)
	fmt.Println("Notes", dpkg.Notes)
	fmt.Println("")
	fmt.Println("Consts")
	for _, v := range dpkg.Consts {
		fmt.Println("	", v.Names, v)
	}
	fmt.Println("Types")
	for _, v := range dpkg.Types {
		fmt.Println("	", v.Name, v)
		for _, m := range v.Methods {
			fmt.Println("		", m.Name, m)
		}
	}
	fmt.Println("Vars")
	for _, v := range dpkg.Vars {
		fmt.Println("	", v.Names, v)
	}
	fmt.Println("Funcs")
	for _, v := range dpkg.Funcs {
		fmt.Println("	", v.Name, v)
	}
	return nil
}

// type Package struct {
// 	Doc        string
// 	Name       string
// 	ImportPath string
// 	Imports    []string
// 	Filenames  []string
// 	Notes      map[string][]*Note

// 	// Deprecated: For backward compatibility Bugs is still populated,
// 	// but all new code should use Notes instead.
// 	Bugs []string

// 	// declarations
// 	Consts []*Value
// 	Types  []*Type
// 	Vars   []*Value
// 	Funcs  []*Func

// 	// Examples is a sorted list of examples associated with
// 	// the package. Examples are extracted from _test.go files
// 	// provided to NewFromFiles.
// 	Examples []*Example
// }
//     Package is the documentation for an entire package.
