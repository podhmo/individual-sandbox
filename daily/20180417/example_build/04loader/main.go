package main

import (
	"errors"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
)

// Loader :
type Loader struct {
	fset     *token.FileSet
	packages map[string]*Package
	build    *build.Context
	cwd      string
	mode     build.ImportMode
}

// Import :
func (l *Loader) Import(pkgpath string) (*Package, error) {
	return l.load(pkgpath, func(bp *build.Package) [][]string {
		return [][]string{
			bp.GoFiles,
		}
	})
}

// ImportWithTests :
func (l *Loader) ImportWithTests(pkgpath string) (*Package, error) {
	return l.load(pkgpath, func(bp *build.Package) [][]string {
		return [][]string{
			bp.GoFiles,
			bp.TestGoFiles,
			bp.XTestGoFiles,
		}
	})
}

// load :
func (l *Loader) load(pkgpath string, candidates func(*build.Package) [][]string) (*Package, error) {
	if pkg, ok := l.packages[pkgpath]; ok {
		return pkg, nil
	}

	bp, err := l.build.Import(pkgpath, l.cwd, l.mode)
	if err != nil {
		return nil, err
	}

	pkg := &Package{
		loader: l,
		Name:   bp.Name,
		Path:   pkgpath,
		Files:  map[string]*ast.File{},
		bp:     bp,
	}
	l.packages[pkgpath] = pkg

	for _, fs := range candidates(bp) {
		for _, f := range fs {
			f := f
			r, err := os.Open(filepath.Join(bp.Dir, f))
			if err != nil {
				return nil, err
			}
			astf, err := parser.ParseFile(l.fset, f, r, parser.ParseComments)
			if err != nil {
				return nil, err
			}
			pkg.Files[f] = astf

		}
	}
	return pkg, nil
}

// Package :
type Package struct {
	loader *Loader
	Name   string
	Path   string
	Files  map[string]*ast.File
	bp     *build.Package
}

// Import :
func (pkg *Package) Import(pkgpath string) (*Package, error) {
	found := false
	for _, imported := range pkg.bp.Imports {
		if imported == pkgpath {
			found = true
		}
	}
	if !found {
		return nil, errors.New("not found")
	}
	return pkg.loader.Import(pkgpath)
}

// ImportWithTests :
func (pkg *Package) ImportWithTests(pkgpath string) (*Package, error) {
	found := false
	for _, imported := range pkg.bp.Imports {
		if imported == pkgpath {
			found = true
		}
	}
	if !found {
		return nil, errors.New("not found")
	}
	return pkg.loader.ImportWithTests(pkgpath)
}

// ImportedPackageNames :
func (pkg *Package) ImportedPackageNames() []string {
	return pkg.bp.Imports
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	cwd, err := os.Getwd()
	if err != nil {
		return err
	}
	loader := &Loader{
		fset:     token.NewFileSet(),
		packages: map[string]*Package{},
		build:    &build.Default,
		cwd:      cwd,
	}

	pkg, err := loader.Import("io")
	if err != nil {
		return err
	}
	fmt.Println(pkg.Name, pkg.Files)
	for _, name := range pkg.ImportedPackageNames() {
		pkg, err := pkg.Import(name)
		if err != nil {
			return err
		}
		fmt.Printf("%#+v\n", pkg)
	}
	return nil
}
