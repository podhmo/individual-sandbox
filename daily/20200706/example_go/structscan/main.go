package main

import (
	"fmt"
	"go/token"
	"go/types"
	"log"
	"os"
	"strings"

	"github.com/pkg/errors"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

// Loader :
type Loader struct {
	fset *token.FileSet
	path string
	syms []string
	mode packages.LoadMode
}

func run(args []string) error {
	path := args[0]

	fset := token.NewFileSet()
	loader := NewLoader(fset, path)
	err := loader.VisitObject(func(pkg *packages.Package, ob types.Object) {
		fmt.Println(pkg, ob)
	})
	if err != nil {
		return err
	}

	return nil
}

// NewLoader
func NewLoader(
	fset *token.FileSet,
	path string,
) *Loader {
	var syms []string
	if strings.Contains(path, ":") {
		splited := strings.SplitN(path, ":", 2)
		path = splited[0]
		syms = append(syms, splited[1])
	}
	return &Loader{
		fset: fset,
		path: path,
		syms: syms,
		mode: packages.NeedName | packages.NeedImports | packages.NeedTypes,
	}
}

// VisitObject :
func (l *Loader) VisitObject(visit func(pkg *packages.Package, ob types.Object)) error {
	cfg := &packages.Config{
		Fset: l.fset,
		Mode: l.mode,
	}
	pkgs, err := packages.Load(cfg, l.path)
	if err != nil {
		return errors.Wrap(err, "load")
	}
	if len(pkgs) != 1 {
		return errors.Wrap(fmt.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs)), "not found")
	}

	pkg := pkgs[0] // using packages.Visit()?
	s := pkg.Types.Scope()
	for _, name := range s.Names() {
		ob := s.Lookup(name)
		if !ob.Exported() {
			continue
		}
		found := true
		name := ob.Id()
		if len(l.syms) > 0 {
			found = false
			for _, sym := range l.syms {
				if name == sym {
					found = true
					break
				}
			}
		}
		if !found {
			continue
		}
		visit(pkg, ob)
	}
	return nil
}
