package main

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/token"
	"go/types"
	"log"
	"os"
	"path"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type MyImporter struct {
	Internal types.Importer
}

// 再帰的なimportをtraceできない
func (i *MyImporter) Import(filepath string) (*types.Package, error) {
	log.Println("import", filepath, "as", path.Base(filepath))
	real, err := i.Internal.Import(filepath)
	return real, err
	
	// // importが途中で打ち切られてしまう。。
	// fake := types.NewPackage(filepath, path.Base(filepath))
	// fake.MarkComplete()
	// fmt.Println(fake)
	// return fake, nil
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
	fmt.Println(pkg)
	importer := &MyImporter{Internal: importer.Default()}

	tpkg := types.NewPackage("./internal/g", "")
	info := &types.Info{
		Uses: map[*ast.Ident]types.Object{},
		Defs: map[*ast.Ident]types.Object{},
	}
	checker := types.NewChecker(&types.Config{
		Importer:                 importer,
		DisableUnusedImportCheck: true,
	}, fset, tpkg, info)

	fmt.Println("Before", tpkg, tpkg.Scope().Names(), info)
	if err := checker.Files(pkg.Syntax); err != nil {
		// return err
		log.Println("!!", err)
	}
	fmt.Println("After ", tpkg, tpkg.Scope().Names(), info)
	fmt.Println("	", tpkg.Scope().Lookup("F"))
	fmt.Println("	", tpkg.Scope().Lookup("G"))
	fmt.Println("	", tpkg.Scope().Lookup("S"))
	fmt.Println("	", (tpkg.Scope().Lookup("S").Type().(*types.Named)).Method(0))
	return nil
}
