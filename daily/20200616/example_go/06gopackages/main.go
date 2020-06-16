package main

import (
	"fmt"
	"go/token"
	"log"
	"os"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	fset := token.NewFileSet()
	pkg, err := LoadPackage(fset, args[0])
	if err != nil {
		return err
	}
	fmt.Println(pkg)
	return nil
}

// LoadPackage :
func LoadPackage(fset *token.FileSet, importPath string) (*packages.Package, error) {
	cfg := &packages.Config{
		Fset: fset,
		// Mode: packages.NeedName,
		Mode: packages.NeedName | packages.NeedImports,
		// Mode: packages.LoadAllSyntax,
		// Mode: packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes,
	}
	pkgs, err := packages.Load(cfg, importPath)
	if err != nil {
		return nil, err
	}
	if len(pkgs) != 1 {
		return nil, errors.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs))
	}
	// encoder := json.NewEncoder(os.Stdout)
	// encoder.SetIndent("", "  ")
	// encoder.Encode(pkgs)
	pp.Println(pkgs)
	return pkgs[0], nil
}
