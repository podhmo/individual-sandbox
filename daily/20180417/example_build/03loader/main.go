package main

import (
	"context"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"sync"

	"golang.org/x/sync/errgroup"
	"golang.org/x/sync/semaphore"
)

// New :
func New(concurrency int) (*Loader, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	return &Loader{
		fset:     token.NewFileSet(),
		packages: map[string]*Package{},
		build:    &build.Default,
		cwd:      cwd,
		sem:      semaphore.NewWeighted(int64(concurrency)),
		OnLoad: func(path string, load func(path string) (*ast.File, error)) (*ast.File, error) {
			log.Printf("load %s", path)
			f, err := load(path)
			if err != nil {
				return nil, err
			}
			log.Printf("done %s", path)
			return f, err
		},
	}, nil
}

// Loader :
type Loader struct {
	fset     *token.FileSet
	packages map[string]*Package
	build    *build.Context
	cwd      string
	mode     build.ImportMode
	sem      *semaphore.Weighted
	OnLoad   func(name string, load func(name string) (*ast.File, error)) (*ast.File, error)
}

// Load :
func (l *Loader) Load(ctx context.Context, pkgpath string) (*Package, error) {
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
	var m sync.Mutex

	filesset := [][]string{
		bp.GoFiles,
		bp.TestGoFiles,
		bp.XTestGoFiles,
	}
	g, ctx := errgroup.WithContext(ctx)
	for _, fs := range filesset {
		for _, f := range fs {
			f := f
			g.Go(func() error {
				l.sem.Acquire(ctx, 1)
				astf, err := l.OnLoad(filepath.Join(bp.Dir, f), func(name string) (*ast.File, error) {
					r, err := os.Open(filepath.Join(bp.Dir, f))
					if err != nil {
						return nil, err
					}
					return parser.ParseFile(l.fset, f, r, parser.ParseComments)
				})
				if err != nil {
					return err
				}
				m.Lock()
				pkg.Files[f] = astf
				m.Unlock()
				l.sem.Release(1)
				return nil
			})
		}
	}
	return pkg, g.Wait()
}

// Package :
type Package struct {
	loader *Loader
	Name   string
	Path   string
	Files  map[string]*ast.File
	bp     *build.Package
}

// Load :
func (pkg *Package) Load(ctx context.Context, pkgpath string) (*Package, error) {
	return pkg.loader.Load(ctx, pkgpath)
}

// Imports :
func (pkg *Package) Imports() []string {
	return pkg.bp.Imports
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	loader, err := New(10)
	if err != nil {
		return err
	}
	ctx := context.Background()
	pkg, err := loader.Load(ctx, "io")
	if err != nil {
		return err
	}
	fmt.Println(pkg.Name, pkg.Files)
	for _, name := range pkg.Imports() {
		fmt.Println(name)
	}
	return nil
}
